package main

import (
	"path"
	"testing"
)

func TestDbInit(t *testing.T) {
	dir := t.TempDir()
	initDb(path.Join(dir, "./test.db"))
}
