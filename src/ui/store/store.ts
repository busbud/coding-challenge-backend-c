import { atom } from "recoil";
import { ISuggestions } from '../types/ISuggestions'
import { Suggestions } from "../types/Suggestions";

export const suggestionsState = atom<ISuggestions|any>({
    key: 'suggestions',
    default: new Suggestions(),
});