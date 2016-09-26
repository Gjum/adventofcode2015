package main
import (
	"io/ioutil"
	"strings"
	"strconv"
)

func main() {
	content, err := ioutil.ReadFile("2.in")
	if err != nil {
		panic(err)
	}

	lines := strings.Split(string(content), "\n")

	var totalArea int
	for _, line := range lines {
		dimension := strings.Split(line, "x")
		x, _ := strconv.Atoi(dimension[0])
		y, _ := strconv.Atoi(dimension[1])
		z, _ := strconv.Atoi(dimension[2])

		cuboid := Cuboid{X:x, Y:y, Z:z}
		area := cuboid.SurfaceAreaWithSlack()

		totalArea += area
	}

	println(totalArea)
}

// Cuboid is a convex polyhedron bounded by six quadrilateral faces,
// whose polyhedral graph is the same as that of a cube.
type Cuboid struct {
	X int
	Y int
	Z int
}

// SurfaceArea calculates the surface area of a cuboid.
func (cuboid *Cuboid) SurfaceArea() int {
	return 2 * (cuboid.X * cuboid.Y + cuboid.X * cuboid.Z + cuboid.Y * cuboid.Z)
}

// SurfaceAreaWithSlack calculates the surface area of a cuboid and adds slack.
func (cuboid *Cuboid) SurfaceAreaWithSlack() int {
	return cuboid.SurfaceArea() + cuboid.X * cuboid.Y
}
