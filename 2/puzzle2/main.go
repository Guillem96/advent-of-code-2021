package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const (
	Forward = "forward"
	Up      = "up"
	Down    = "down"
)

type Command struct {
	command string
	units   int
}

func CommandFromLine(line string) (*Command, error) {
	l := strings.TrimSpace(line)
	splits := strings.Split(l, " ")
	if len(splits) != 2 {
		errMsg := fmt.Sprintf("invalid line %s", line)
		return nil, errors.New(errMsg)
	}

	c := splits[0]
	if c != Forward && c != Up && c != Down {
		errMsg := fmt.Sprintf("invalid command %s", c)
		return nil, errors.New(errMsg)
	}

	u, err := strconv.Atoi(splits[1])
	if err != nil {
		return nil, err
	}

	return &Command{
		command: c,
		units:   u,
	}, nil
}

type Submarine struct {
	horizontal int
	depth      int
	aim        int
}

func (s *Submarine) ApplyCommand(c *Command) {
	if c.command == Forward {
		s.horizontal += c.units
		s.depth += c.units * s.aim
	} else if c.command == Up {
		s.aim -= c.units
	} else {
		s.aim += c.units
	}
}

func (s *Submarine) Result() int {
	return s.horizontal * s.depth
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error reading input file.")
		os.Exit(1)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	sub := &Submarine{horizontal: 0, depth: 0, aim: 0}
	for scanner.Scan() {
		cmd, err := CommandFromLine(scanner.Text())
		if err != nil {
			fmt.Printf("Error parsing command:", err)
			os.Exit(1)
		}
		sub.ApplyCommand(cmd)
	}

	fmt.Println("Result:", sub.Result())
}
