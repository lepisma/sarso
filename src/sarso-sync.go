package main

import (
	"encoding/json"
	"fmt"
	"github.com/cheggaaa/pb/v3"
	"github.com/jessevdk/go-flags"
	jira "gopkg.in/andygrunwald/go-jira.v1"
	"io/ioutil"
	"os"
)

func initDb(dbPath string) {
	fmt.Println(":: Initializing database")
}

func countIssues(client *jira.Client, project *jira.Project) int {
	_, resp, err := client.Issue.Search("project="+project.Key, &jira.SearchOptions{MaxResults: 0})
	if err != nil {
		panic(err)
	}

	return resp.Total
}

func writeToDb(issues []jira.Issue, dbPath string) {
	outputB, err := json.Marshal(issues)
	if err != nil {
		panic(err)
	}
	err = ioutil.WriteFile(dbPath, outputB, 0644)
	if err != nil {
		panic(err)
	}
}

var opts struct {
	DbPath      string `short:"f" long:"file" description:"Database file" value-name:"FILE" required:"true"`
	ProjectName string `short:"p" long:"project" description:"Name of the project to sync" required:"true"`
}

func main() {
	_, err := flags.Parse(&opts)
	if err != nil {
		panic(err)
	}

	base := os.Getenv("JIRA_BASE")
	username := os.Getenv("JIRA_USER")
	token := os.Getenv("JIRA_TOKEN")

	tp := jira.BasicAuthTransport{
		Username: username,
		Password: token,
	}

	client, err := jira.NewClient(tp.Client(), base)
	if err != nil {
		panic(err)
	}

	if _, err := os.Stat(opts.DbPath); os.IsNotExist(err) {
		initDb(opts.DbPath)
	}

	project, _, err := client.Project.Get(opts.ProjectName)
	if err != nil {
		panic(err)
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
	if err != nil {
		panic(err)
	}

	bar.Finish()

	writeToDb(issues, opts.DbPath)
}
