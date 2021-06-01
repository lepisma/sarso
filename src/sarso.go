package main

import (
	"database/sql"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/cheggaaa/pb/v3"
	"github.com/docopt/docopt-go"
	_ "github.com/mattn/go-sqlite3"
	"github.com/mitchellh/go-homedir"
	jira "gopkg.in/andygrunwald/go-jira.v1"
)

const Version = "1.1.0"

func cry(err error) {
	if err != nil {
		panic(err)
	}
}

func initDb(dbPath string) {
	fmt.Println(":: Initializing database")
	db, err := sql.Open("sqlite3", dbPath)
	cry(err)

	defer db.Close()

	_, err = db.Exec(`CREATE TABLE issues (
        id INTEGER PRIMARY KEY,
        key TEXT UNIQUE,
        summary TEXT NOT NULL,
        description TEXT,
        type TEXT,
        status TEXT,
        resolution TEXT,
        assignee_id TEXT,
        duedate TEXT,
        fields TEXT,
        FOREIGN KEY(assignee_id) REFERENCES users(account_id)
    )`)
	cry(err)

	_, err = db.Exec(`CREATE TABLE users (
        account_id text UNIQUE,
        email TEXT UNIQUE,
        display_name TEXT NOT NULL
    )`)
	cry(err)
}

func purgeDb(dbPath string) {
	db, err := sql.Open("sqlite3", dbPath)
	cry(err)

	defer db.Close()

	_, err = db.Exec(`DELETE FROM issues`)
	cry(err)

	_, err = db.Exec(`DELETE FROM users`)
	cry(err)
}

// Create a new Jira issue without any assignee
func createIssue(client *jira.Client, project *jira.Project, summary string, description string, issueType string) *jira.Issue {
	i := jira.Issue{
		Fields: &jira.IssueFields{
			Description: description,
			Project: jira.Project{
				Key: project.Key,
			},
			Type: jira.IssueType{
				Name: issueType,
			},
			Summary: summary,
		},
	}
	issue, _, err := client.Issue.Create(&i)
	cry(err)
	return issue
}

// Read issues with no key from the sarso database and push to Jira
func pushIssues(dbPath string, client *jira.Client, project *jira.Project) {
	db, err := sql.Open("sqlite3", dbPath)
	cry(err)

	defer db.Close()

	rows, err := db.Query("SELECT id, summary, description, type FROM issues WHERE key IS NULL")
	cry(err)

	var id int
	var summary string
	var description string
	var issueType string

	var ids []int
	var issues []*jira.Issue

	for rows.Next() {
		err = rows.Scan(&id, &summary, &description, &issueType)
		cry(err)

		issues = append(issues, createIssue(client, project, summary, description, issueType))
		ids = append(ids, id)
	}
	rows.Close()

	if len(issues) > 0 {
		fmt.Printf(":: Creating %d new issue(s)\n", len(issues))

		updateStmt, err := db.Prepare("UPDATE issues SET key = ? WHERE id = ?")
		cry(err)

		for i, issue := range issues {
			_, err = updateStmt.Exec(issue.Key, ids[i])
			cry(err)
		}
	}

}

// Save issues in the sarso database. Also save assigned users in the issues.
func writeToDb(issues []jira.Issue, dbPath string) {
	db, err := sql.Open("sqlite3", dbPath)
	cry(err)

	defer db.Close()

	user_stmt, err := db.Prepare(`REPLACE INTO users(
	    account_id,
	    email,
	    display_name
	    ) VALUES(?, ?, ?)
	`)
	cry(err)

	issue_stmt, err := db.Prepare(`REPLACE INTO issues(
        key,
        summary,
        description,
        type,
        status,
        resolution,
        assignee_id,
        duedate,
        fields
        ) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)
    `)
	cry(err)

	var statusString, resolutionString, assigneeId *string

	for _, issue := range issues {
		if issue.Fields.Status == nil {
			statusString = nil
		} else {
			statusString = &issue.Fields.Status.Name
		}

		if issue.Fields.Resolution == nil {
			resolutionString = nil
		} else {
			resolutionString = &issue.Fields.Resolution.Name
		}

		if issue.Fields.Assignee == nil {
			assigneeId = nil
		} else {
			assigneeId = &issue.Fields.Assignee.AccountID
			_, err = user_stmt.Exec(
				assigneeId,
				issue.Fields.Assignee.EmailAddress,
				issue.Fields.Assignee.DisplayName,
			)
			cry(err)
		}

		fieldsBytes, err := issue.Fields.MarshalJSON()
		cry(err)

		_, err = issue_stmt.Exec(
			issue.Key,
			issue.Fields.Summary,
			issue.Fields.Description,
			issue.Fields.Type.Name,
			statusString,
			resolutionString,
			assigneeId,
			time.Time(issue.Fields.Duedate).Format(time.RFC3339),
			fieldsBytes,
		)
		cry(err)
	}
}

func countIssues(client *jira.Client, jql string) int {
	_, resp, err := client.Issue.Search(jql, &jira.SearchOptions{MaxResults: 0})
	cry(err)

	return resp.Total
}

func buildJql(projectKeys []string) string {
	var projectTerms []string
	for _, k := range projectKeys {
		projectTerms = append(projectTerms, "project = "+k)
	}

	return strings.Join(projectTerms, " OR ")
}

func main() {
	usage := `sarso

Usage:
  sarso sync <project-keys>... [--push-only] [--db-path=<db-path>]
  sarso purge [--db-path=<db-path>]
  sarso -h | --help
  sarso --version

Arguments:
  <project-keys>                  Jira project keys.

Options:
  -h --help                       Show this screen.
  --version                       Show version.
  --db-path=<db-path>             Database file path [default: ~/.sarso.sqlite].
  --push-only                     Only push tickets from sarso to Jira and
                                  not the other way.
`

	arguments, _ := docopt.ParseArgs(usage, os.Args[1:], Version)
	dbPath, _ := arguments["--db-path"].(string)
	dbPath, _ = homedir.Expand(dbPath)

	if _, err := os.Stat(dbPath); os.IsNotExist(err) {
		initDb(dbPath)
	}

	if purgeArg, _ := arguments["purge"].(bool); purgeArg {
		purgeDb(dbPath)
		fmt.Println(":: Database purged")
	} else if syncArg, _ := arguments["sync"].(bool); syncArg {
		projectKeys, _ := arguments["<project-keys>"].([]string)
		pushOnly, _ := arguments["--push-only"].(bool)

		base := os.Getenv("JIRA_BASE")
		username := os.Getenv("JIRA_USER")
		token := os.Getenv("JIRA_TOKEN")

		tp := jira.BasicAuthTransport{
			Username: username,
			Password: token,
		}

		client, err := jira.NewClient(tp.Client(), base)
		cry(err)

		if pushOnly {
			panic("Push only not implemented")
		}

		jql := buildJql(projectKeys)

		totalIssues := countIssues(client, jql)
		fmt.Printf(":: Found total %d issues\n", totalIssues)
		bar := pb.StartNew(totalIssues)

		var issues []jira.Issue
		appendFunc := func(i jira.Issue) (err error) {
			issues = append(issues, i)
			bar.Increment()
			return err
		}

		err = client.Issue.SearchPages(jql, nil, appendFunc)
		cry(err)

		bar.Finish()

		writeToDb(issues, dbPath)
	}
}
