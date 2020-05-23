package main

import (
	// "database/sql"
	"fmt"
	"github.com/jessevdk/go-flags"
	// _ "github.com/mattn/go-sqlite3"
	jira "gopkg.in/andygrunwald/go-jira.v1"
	"os"
)

func initDb(dbPath string) {
	fmt.Println(":: Initializing database")
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

	_, err = jira.NewClient(tp.Client(), base)
	if err != nil {
		panic(err)
	}

	if _, err := os.Stat(opts.DbPath); os.IsNotExist(err) {
		initDb(opts.DbPath)
	}
}
