package main

import (
	// "database/sql"
	"fmt"
	// _ "github.com/mattn/go-sqlite3"
	jira "gopkg.in/andygrunwald/go-jira.v1"
	"os"
)

func main() {
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

	// argsWithoutProg := os.Args[1:]
	fmt.Print(client)
}
