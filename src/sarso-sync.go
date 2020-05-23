package main

import (
	"database/sql"
	"fmt"
	"github.com/cheggaaa/pb/v3"
	"github.com/jessevdk/go-flags"
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

	stmt, err := db.Prepare(`CREATE TABLE issues (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        key TEXT UNIQUE,
        summary TEXT NOT NULL,
        description TEXT
    )`)
	cry(err)

	_, err = stmt.Exec()
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

var opts struct {
	DbPath      string `short:"f" long:"file" description:"Database file" value-name:"FILE" required:"true"`
	ProjectName string `short:"p" long:"project" description:"Name of the project to sync" required:"true"`
}

func main() {
	_, err := flags.Parse(&opts)
	cry(err)

	base := os.Getenv("JIRA_BASE")
	username := os.Getenv("JIRA_USER")
	token := os.Getenv("JIRA_TOKEN")

	tp := jira.BasicAuthTransport{
		Username: username,
		Password: token,
	}

	client, err := jira.NewClient(tp.Client(), base)
	cry(err)

	if _, err := os.Stat(opts.DbPath); os.IsNotExist(err) {
		initDb(opts.DbPath)
	}

	project, _, err := client.Project.Get(opts.ProjectName)
	cry(err)

	pushIssues(opts.DbPath, client, project)

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

	writeToDb(issues, opts.DbPath)
}
