all: ./build/sarso-sync

install: ./build/sarso-sync
	install -m +x ./build/sarso-sync /usr/local/bin

./build/sarso-sync: ./src/sarso-sync.go
	go build -o ./build/sarso-sync ./src/sarso-sync.go
