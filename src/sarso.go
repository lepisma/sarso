package main

import (
	"database/sql"
	"fmt"
	"github.com/cheggaaa/pb/v3"
	"github.com/docopt/docopt-go"
	_ "github.com/mattn/go-sqlite3"
	jira "gopkg.in/andygrunwald/go-jira.v1"
	"os"
)

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
        description TEXT
    )`)
	cry(err)
}

func purgeDb(dbPath string) {
	db, err := sql.Open("sqlite3", dbPath)
	cry(err)

	defer db.Close()

	_, err = db.Exec(`DELETE FROM issues`)
	cry(err)
}

func createIssue(client *jira.Client, project *jira.Project, summary string, description string) *jira.Issue {
	i := jira.Issue{
		Fields: &jira.IssueFields{
			Description: description,
			Project: jira.Project{
				Key: project.Key,
			},
			Type: jira.IssueType{
				Name: "Task",
			},
			Summary: summary,
		},
	}
	issue, _, err := client.Issue.Create(&i)
	cry(err)
	return issue
}

func pushIssues(dbPath string, client *jira.Client, project *jira.Project) {
	db, err := sql.Open("sqlite3", dbPath)
	cry(err)

	defer db.Close()

	rows, err := db.Query("SELECT id, summary, description FROM issues WHERE key IS NULL")
	cry(err)

	var id int
	var summary string
	var description string

	var ids []int
	var issues []*jira.Issue

	for rows.Next() {
		err = rows.Scan(&id, &summary, &description)
		cry(err)

		issues = append(issues, createIssue(client, project, summary, description))
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

func writeToDb(issues []jira.Issue, dbPath string) {
	db, err := sql.Open("sqlite3", dbPath)
	cry(err)

	defer db.Close()

	stmt, err := db.Prepare("REPLACE INTO issues(key, summary, description) VALUES(?, ?, ?)")
	cry(err)

	for _, issue := range issues {
		_, err = stmt.Exec(issue.Key, issue.Fields.Summary, issue.Fields.Description)
		cry(err)
	}
}

func countIssues(client *jira.Client, project *jira.Project) int {
	_, resp, err := client.Issue.Search("project="+project.Key, &jira.SearchOptions{MaxResults: 0})
	cry(err)

	return resp.Total
}

func main() {
	usage := `sarso

Usage:
  sarso sync --db-path=<db-path> --project-key=<project-key> [--push-only]
  sarso purge --db-path=<db-path>
  sarso -h | --help
  sarso --version

Options:
  -h --help                     Show this screen.
  --version                     Show version.
  --db-path=<db-path>           Database file path.
  --project-key=<project-key>   Project key from Jira.
  --push-only                   Only push tickets from sarso to Jira and
                                not the other way.
`

	arguments, _ := docopt.ParseArgs(usage, os.Args[1:], "0.2.0")
	dbPath, _ := arguments["--db-path"].(string)

	if _, err := os.Stat(dbPath); os.IsNotExist(err) {
		initDb(dbPath)
	}

	if purgeArg, _ := arguments["purge"].(bool); purgeArg {
		purgeDb(dbPath)
		fmt.Println(":: Database purged")
	} else if syncArg, _ := arguments["sync"].(bool); syncArg {
		//
		projectKey, _ := arguments["--project-key"].(string)
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

		project, _, err := client.Project.Get(projectKey)
		cry(err)

		pushIssues(dbPath, client, project)
		if pushOnly {
			return
		}

		totalIssues := countIssues(client, project)
		fmt.Printf(":: Found total %d issues in project %s\n", totalIssues, project.Key)
		bar := pb.StartNew(totalIssues)

		var issues []jira.Issue
		appendFunc := func(i jira.Issue) (err error) {
			issues = append(issues, i)
			bar.Increment()
			return err
		}

		err = client.Issue.SearchPages("project="+project.Key, nil, appendFunc)
		cry(err)

		bar.Finish()

		writeToDb(issues, dbPath)
	}
}
