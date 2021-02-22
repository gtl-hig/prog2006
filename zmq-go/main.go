package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"

	zmq "github.com/pebbe/zmq4"
)

func main() {
	pub := flag.Bool("pub", false, "run as publisher")
	name := flag.String("name", "Mariusz (Go): ", "name for your publisher")

	flag.Parse()

	if *pub {
		publisher, _ := zmq.NewSocket(zmq.PUB)
		defer publisher.Close()

		publisher.Bind("tcp://*:5555")
		scanner := bufio.NewScanner(os.Stdin)
		for scanner.Scan() {
			publisher.Send(*name+scanner.Text(), 0)
		}
		return
	}

	// We are a subscriber
	subscriber, _ := zmq.NewSocket(zmq.SUB)
	defer subscriber.Close()
	subscriber.Connect("tcp://127.0.0.1:5555")
	subscriber.SetSubscribe("")

	for {
		msg, e := subscriber.Recv(0)
		if e != nil {
			log.Println("ERROR: ", e)
			break
		}
		if msg == "END" {
			break
		}

		fmt.Println(msg)
	}
}
