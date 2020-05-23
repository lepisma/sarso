all: ./build/sarso

.PHONY: clean
clean:
	rm ./build/sarso

install: ./build/sarso
	install -m +x ./build/sarso /usr/local/bin

./build/sarso: ./src/sarso.go
	go build -o ./build/sarso ./src/sarso.go
