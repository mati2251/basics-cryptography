package main

import (
	"fmt"
	"math/big"
	"math/rand/v2"
	"strings"
)

func sieveOfEratosthenes(n int) []int {
	primeTest := make([]bool, n+1)
	for i := 2; i <= n; i++ {
		primeTest[i] = true
	}
	for i := 2; i <= n; i++ {
		if primeTest[i] == true {
			for j := i * i; j <= n; j += i {
				primeTest[j] = false
			}
		}
	}
	prime := []int{}
	for i := 2; i <= n; i++ {
		if primeTest[i] == true {
			prime = append(prime, i)
		}
	}
	return prime
}

func getN(prime []int) int {
	p := 0
	for i := len(prime) - 1; i > 0; i-- {
		if prime[i]%4 == 3 {
			if p == 0 {
				p = prime[i]
			}
			return p * prime[i]
		}
	}
	return 0
}

func nwd(a int, b int) int {
	if b == 0 {
		return a
	}
	return nwd(b, a%b)
}

func randomX(n int) int {
	x := rand.IntN(n)
	for ok := true; ok; ok = nwd(n, x) != 1 {
		x += 1
	}
	return x
}

func randomNumber(bitsCount int) (big.Int, []bool) {
	var number big.Int
	bits := make([]bool, bitsCount)
	number.SetBit(&number, 0, 0)
	primes := sieveOfEratosthenes(1000000000)
	n := getN(primes)
	for i := 0; i < bitsCount; i++ {
		x := randomX(n)
		var lsb uint
		lsb = uint(x & 1)
		bits[i] = lsb != 0
		number.SetBit(&number, i, lsb)
	}
	return number, bits
}

func testOne(numberBinary string) {
	fmt.Println("Test 1: ")
	oneCount := strings.Count(numberBinary, "1")
	fmt.Printf("Number of 1s: %d\n", oneCount)
}

func testTwoAndThree(numberBinary string) {
	// (20000 - (2685 + 1386 * 2 + 723 * 3 + 384 * 4 + 209 * 5))/209 = 49
	// good interval ???
	// test 2 is passed if test 3 is failed
	fmt.Println("Test 2 (6 means 6>=): ")
	countSeries := countSeriesOfBits(numberBinary)
	test3 := "Passed"
	for k, v := range countSeries {
		if k > 25 {
			test3 = "Failed"
		}
		if k > 6 {
			countSeries[6] += v
			delete(countSeries, k)
		}
	}
	fmt.Println(countSeries)
	fmt.Println("Test 3:", test3)
}

func countSeriesOfBits(numberBinary string) map[int]int {
	counts := make(map[int]int)
	currentCount := 1
	currentBit := numberBinary[0]
	for i := 1; i < len(numberBinary); i++ {
		if numberBinary[i] == currentBit {
			currentCount++
		}
		if numberBinary[i] != currentBit || i == len(numberBinary)-1 {
			counts[currentCount]++
			currentCount = 1
			currentBit = numberBinary[i]
		}
	}
	return counts
}

func testFour(numberBinary string) {
	fmt.Print("Test 4: ")
	for i := 0; i < len(numberBinary)/5000; i++ {
		currentString := numberBinary[i*5000 : (i+1)*5000]
		counts := make(map[int]int)
		for j := 0; j < 16; j++ {
			binaryString := fmt.Sprintf("%04b", j)
			counts[j] = strings.Count(currentString, binaryString)
		}
		var sum float32
		sum = 0
		for _, v := range counts {
			sum += float32(v * v)
		}
		sum = (sum*16)/5000 - 5000
		if i%100 == 0 {
			fmt.Print(sum, " ")
		}
		if sum < 2.16 || sum > 46.17 {
			fmt.Println("Failed")
			return
		}
	}
	fmt.Println("Passed")
}

func main() {
	random, _ := randomNumber(20000)
	randomString := random.Text(2)
	fmt.Println(random.String())
	testOne(randomString)
	testTwoAndThree(randomString)
	testFour(randomString)
}
