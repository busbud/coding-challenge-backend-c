import React from "react"
import { Card } from 'react-bootstrap'
import { suggestionsState } from "./store/store"
import { useRecoilState } from "recoil";

export function SuggestionResult (): JSX.Element {
    
    const [suggestionsResult, setSuggestionsResult] = useRecoilState(suggestionsState)

    const suggestionsCards = suggestionsResult.suggestions.map((suggestion, index) => <Card key={index} style={{cursor: "pointer", marginBottom: "10px"}}>
            <Card.Body>
                <Card.Text>
                    {suggestion.name}
                </Card.Text>
            </Card.Body>
        </Card> )

    return suggestionsCards
}