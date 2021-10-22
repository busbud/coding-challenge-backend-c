import React, { useState } from "react"
import axios from "axios";
import {Button, Container, FormControl, InputGroup, Navbar, Row} from 'react-bootstrap'
import { suggestionsState } from "./store/store"
import { ISuggestions } from './types/ISuggestions'
import { useRecoilState } from "recoil";
import { SuggestionResult } from "./SuggestionResult"

export function SearchCities(): JSX.Element {
    
    const [inputValue, setInputValue] = useState('')
    const [suggestions, setSuggestions] = useRecoilState(suggestionsState)

    const onChange = ({target: {value}}) => {
        setInputValue(value);
    };

    const onClick = () => {
        axios.get<ISuggestions>(`/suggestions?q=${inputValue}`)
            .then((res) => {
                setSuggestions(res.data)
            })
            .catch((error) => {
                console.error(error)
        })
    }

    const searchCitiesContents = 
        <>
            <Navbar bg="dark" expand="lg" variant="dark">
                <Container>
                    <Navbar.Brand href="/" >Busbud Code Challenge</Navbar.Brand>
                </Container>
            </Navbar>
            <Container style={{marginTop: "40px"}}>
                <Row>
                    <InputGroup className="mb-3">
                        <FormControl
                            placeholder="Search Cities"
                            aria-label="Search Cities"
                            onChange={onChange}
                        />
                        <Button 
                            variant="outline-primary" 
                            id="button-search1" 
                            onClick={onClick}>
                            Search Cities
                        </Button>
                    </InputGroup>
                </Row>
                <Row style={{marginTop: "40px"}}>
                    <SuggestionResult />
                </Row>
            </Container>
        </>
        

    return searchCitiesContents
}