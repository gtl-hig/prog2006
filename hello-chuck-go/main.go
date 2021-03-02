package main

import (
	"encoding/json"
	"fmt"
	"net/http"
)

// Chucknorris is the struct used to  unmarshal the JSON response from the URL
type Joke struct {
	Category []string `json:"category"`
	IconURL  string   `json:"icon_url"`
	ID       string   `json:"id"`
	URL      string   `json:"url"`
	Value    string   `json:"value"`
}

// It would have been nice if Go differentiate between constants and regular variables
const chuckAPI = "http://api.chucknorris.io/jokes/random"

func main() {
	req, err := http.NewRequest("GET", chuckAPI, nil)
	if err != nil {
		fmt.Printf("%v", err)
		return
	}

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Printf("%v", err)
		return
	}
	defer resp.Body.Close()

	var joke Joke
	err = json.NewDecoder(resp.Body).Decode(&joke)
	// The above line is the same as
	// respData, err := ioutil.ReadAll(resp.Body)
	// if err != nil {
	//	return joke, fmt.Errorf("Read error")
	//}
	//if err = json.Unmarshal(respData, &joke); err != nil {
	//	return joke, fmt.Errorf("Error in unmarsheling, %v", err)
	//}
	if err != nil {
		fmt.Printf("%v", err)
	}

	fmt.Printf("%s\n", joke.Value)
}
