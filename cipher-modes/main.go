package main

import (
	"crypto/aes"
	"crypto/cipher"
	"fmt"
	"os"
	"strings"
	"time"
)

var resultFile *os.File

func addScore(ecbEncryptionTime time.Duration, ecbDecryptionTime time.Duration, cbcEncryptionTime time.Duration, cbcDecryptionTime time.Duration, ofbEncryptionTime time.Duration, ofbDecryptionTime time.Duration, cfbEncryptionTime time.Duration, cfbDecryptionTime time.Duration, ctrEncryptionTime time.Duration, ctrDecryptionTime time.Duration) {
	resultFile.WriteString(fmt.Sprintf("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n", ecbEncryptionTime, cbcEncryptionTime, ofbEncryptionTime, cfbEncryptionTime, ctrEncryptionTime, ecbDecryptionTime, cbcDecryptionTime, ofbDecryptionTime, cfbDecryptionTime, ctrDecryptionTime))
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func readFiles() []*os.File {
	fileNames := []string{"random_file_10KB.txt", "random_file_100KB.txt", "random_file_1MB.txt", "random_file_10MB.txt", "random_file_100MB.txt", "random_file_1GB.txt"}
	files := make([]*os.File, len(fileNames))
	for index, fileName := range fileNames {
		file, err := os.Open(fileName)
		check(err)
		files[index] = file
	}
	return files
}

func timeEncryptionComparison(content []byte, key []byte, iv []byte, size int64) {
	fmt.Println("Time encryption comparison")
	c, err := aes.NewCipher(key)
	check(err)
	startTime := time.Now()
	ecbEncryption(c, content)
	ecbEncryptionTime := time.Since(startTime)
	fmt.Printf("Time to encrypt: %s\n", ecbEncryptionTime)
	startTime = time.Now()
	ecbDecryption(c, content)
	ecbDecryptionTime := time.Since(startTime)
	fmt.Printf("Time to decrypt: %s\n", ecbDecryptionTime)

	fmt.Printf("CBC mode\n")
	cbcEncryptionTime, cbcDecryptionTime := timeEncryptionBlock(content, cipher.NewCBCEncrypter(c, iv), cipher.NewCBCDecrypter(c, iv))

	fmt.Printf("OFB mode\n")
	ofbEncryptionTime, ofbDecryptionTime := timeEncryptionStream(content, cipher.NewOFB(c, iv), cipher.NewOFB(c, iv))

	fmt.Printf("CFB mode\n")
	cfbEncryptionTime, cfbDecryptionTime := timeEncryptionStream(content, cipher.NewCFBEncrypter(c, iv), cipher.NewCFBDecrypter(c, iv))

	fmt.Printf("CTR mode\n")
	ctrEncryptionTime, ctrDecryptionTime := timeEncryptionStream(content, cipher.NewCTR(c, iv), cipher.NewCTR(c, iv))

	addScore(ecbEncryptionTime, ecbDecryptionTime, cbcEncryptionTime, cbcDecryptionTime, ofbEncryptionTime, ofbDecryptionTime, cfbEncryptionTime, cfbDecryptionTime, ctrEncryptionTime, ctrDecryptionTime)
	fmt.Printf("\n")
}

func timeEncryptionBlock(content []byte, encryption cipher.BlockMode, decryption cipher.BlockMode) (time.Duration, time.Duration) {
	fmt.Println("Time encryption")
	startTime := time.Now()
	decrypted := make([]byte, len(content))
	encryption.CryptBlocks(decrypted, content)
	encryptionTime := time.Since(startTime)
	fmt.Printf("Time to encrypt: %s\n", encryptionTime)
	startTime = time.Now()
	decryption.CryptBlocks(decrypted, content)
	decryptionTime := time.Since(startTime)
	fmt.Printf("Time to decrypt: %s\n", decryptionTime)
	return encryptionTime, decryptionTime
}

func timeEncryptionStream(content []byte, encryption cipher.Stream, decryption cipher.Stream) (time.Duration, time.Duration) {
	fmt.Println("Time encryption")
	startTime := time.Now()
	decrypted := make([]byte, len(content))
	encryption.XORKeyStream(decrypted, content)
	encryptionTime := time.Since(startTime)
	fmt.Printf("Time to encrypt: %s\n", encryptionTime)
	startTime = time.Now()
	decryption.XORKeyStream(decrypted, content)
	decryptionTime := time.Since(startTime)
	fmt.Printf("Time to decrypt: %s\n", decryptionTime)
	return encryptionTime, decryptionTime
}

func errorPropagationComprasion() {
	fmt.Println("Error propagation")
	key := []byte("1234567890123456")
	iv := []byte("1234567890123456")
	c, err := aes.NewCipher(key)
	check(err)
	content := []byte("Lorem ipsum dolor sit amet, consectetur adipiscing elit blandit.")
	badContent := []byte("Lorem ipsum iolor sit amet, consectetur adipiscing elit blandit.")
	badEncrypted := make([]byte, len(content))
	encrypted := make([]byte, len(content))
	decrypted := make([]byte, len(content))

	fmt.Println("ECB mode")
	badEncrypted = ecbEncryption(c, badContent)
	encrypted = ecbEncryption(c, content)
	fmt.Printf("Bad encryption: %08b\n", badEncrypted)
	fmt.Printf("Valid encryption: %08b\n", encrypted)
	err = os.WriteFile("errors/bad_encrypted_ecb.txt", badEncrypted, 0644)
	check(err)
	err = os.WriteFile("errors/encrypted_ecb.txt", encrypted, 0644)
	check(err)
	encrypted[17] = 0
	encrypted[18] = 0
	encrypted[19] = 0
	decrypted = ecbDecryption(c, encrypted)
	fmt.Printf("Bad decryption: %s\n", string(decrypted))
	fmt.Printf("Valid decryption: %s\n", string(content))
	err = os.WriteFile("errors/bad_decrypted_ecb.txt", decrypted, 0644)
	check(err)
	err = os.WriteFile("errors/decrypted_ecb.txt", content, 0644)
	check(err)

	fmt.Println("CBC mode")
	encryption := cipher.NewCBCEncrypter(c, iv)
	decryption := cipher.NewCBCDecrypter(c, iv)
	encryption.CryptBlocks(badEncrypted, badContent)
	encryption.CryptBlocks(encrypted, content)
	fmt.Printf("Bad encryption: %08b\n", badEncrypted)
	fmt.Printf("Valid encryption: %08b\n", encrypted)
	err = os.WriteFile("errors/bad_encrypted_cbc.txt", badEncrypted, 0644)
	check(err)
	err = os.WriteFile("errors/encrypted_cbc.txt", encrypted, 0644)
	check(err)
	encrypted[17] = 0
	encrypted[18] = 0
	encrypted[19] = 0
	decryption.CryptBlocks(decrypted, encrypted)
	fmt.Printf("Bad decryption: %s\n", string(decrypted))
	fmt.Printf("Valid decryption: %s\n", string(content))
	err = os.WriteFile("errors/bad_decrypted_cbc.txt", decrypted, 0644)
	check(err)
	err = os.WriteFile("errors/decrypted_cbc.txt", content, 0644)
	check(err)

	errorPropagationStream(cipher.NewOFB(c, iv), cipher.NewOFB(c, iv), content, badContent, "OFB")

	errorPropagationStream(cipher.NewCFBEncrypter(c, iv), cipher.NewCFBDecrypter(c, iv), content, badContent, "CFB")

	errorPropagationStream(cipher.NewCTR(c, iv), cipher.NewCTR(c, iv), content, badContent, "CTR")

	fmt.Println()
}

func errorPropagationStream(encryption cipher.Stream, decryption cipher.Stream, content []byte, badContent []byte, mode string) {
	fmt.Printf("%s mode\n", mode)
	decrypted := make([]byte, len(content))
	encrypted := make([]byte, len(content))
	badEncrypted := make([]byte, len(content))

	encryption.XORKeyStream(badEncrypted, badContent)
	encryption.XORKeyStream(encrypted, content)
	fmt.Printf("Bad encryption: %08b\n", badEncrypted)
	fmt.Printf("Valid encryption: %08b\n", encrypted)
	err := os.WriteFile("errors/bad_encrypted_"+strings.ToLower(mode)+".txt", badEncrypted, 0644)
	check(err)
	err = os.WriteFile("errors/encrypted_"+strings.ToLower(mode)+".txt", encrypted, 0644)

	encrypted[17] = 0
	encrypted[18] = 0
	encrypted[19] = 0
	decryption.XORKeyStream(decrypted, encrypted)
	fmt.Printf("Bad decryption: %s\n", string(decrypted))
	fmt.Printf("Valid decryption: %s\n", string(content))
	err = os.WriteFile("errors/bad_decrypted_"+strings.ToLower(mode)+".txt", decrypted, 0644)
	check(err)
	err = os.WriteFile("errors/decrypted_"+strings.ToLower(mode)+".txt", content, 0644)
}

func ecbEncryption(block cipher.Block, content []byte) []byte {
	encrypted := make([]byte, len(content))
	blockSize := 16
	for bs, be := 0, blockSize; bs < len(content); bs, be = bs+blockSize, be+blockSize {
		block.Encrypt(encrypted[bs:be], content[:16])
		content = content[16:]
	}
	return encrypted
}

func ecbDecryption(block cipher.Block, content []byte) []byte {
	decrypted := make([]byte, len(content))
	blockSize := 16
	for bs, be := 0, blockSize; bs < len(content); bs, be = bs+blockSize, be+blockSize {
		block.Decrypt(decrypted[bs:be], content[16:])
		content = content[16:]
	}
	return decrypted
}

func cbcFromECB() {
	fmt.Println("CBC from ECB")
	block, err := aes.NewCipher([]byte("1234567890123456"))
	check(err)
	iv := []byte("1234567890123456")
	content := []byte("Lorem ipsum dolor sit amet, consectetur adipiscing elit blandit.")
	encrypted := cbcEncryption(block, iv, content)
	fmt.Printf("CBC: %s\n", encrypted)
	decrypted := cbcDecryption(block, iv, encrypted)
	fmt.Printf("Decrypted: %s\n", string(decrypted))
}

func cbcEncryption(block cipher.Block, iv []byte, content []byte) []byte {
	encrypted := make([]byte, len(content))
	blockSize := 16
	for bs, be := 0, blockSize; bs < len(content); bs, be = bs+blockSize, be+blockSize {
		block.Encrypt(encrypted[bs:be], content[bs:be])
		for i := 0; i < blockSize; i++ {
			content[bs+i] ^= iv[i]
		}
		copy(iv, encrypted[bs:be])
	}
	return encrypted
}

func cbcDecryption(block cipher.Block, iv []byte, content []byte) []byte {
	decrypted := make([]byte, len(content))
	blockSize := 16
	for bs, be := 0, blockSize; bs < len(content); bs, be = bs+blockSize, be+blockSize {
		for i := 0; i < blockSize; i++ {
			decrypted[bs+i] ^= iv[i]
		}
		block.Decrypt(decrypted[bs:be], content[bs:be])
		copy(iv, content[bs:be])
	}
	return decrypted
}

func main() {
	files := readFiles()
	resultFile, _ = os.Create("results.csv")
	for _, file := range files {
		stats, err := file.Stat()
		check(err)
		fmt.Printf("File: %s, Size: %d bytes\n", stats.Name(), stats.Size())
		content := make([]byte, stats.Size())
		_, err = file.Read(content)
		check(err)
		key := []byte("1234567890123456")
		iv := []byte("1234567890123456")
		timeEncryptionComparison(content, key, iv, stats.Size())
	}

	errorPropagationComprasion()

	cbcFromECB()
}