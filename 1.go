package main
import (
	"io/ioutil"
)

func main() {
	content, err := ioutil.ReadFile("1.in")
	if err != nil {
		panic(err)
	}

	var floor int
	for _, char := range content {
		if string(char) == "(" {
			floor++
		}

		if string(char) == ")" {
			floor--
		}
	}

	println(floor)
}
