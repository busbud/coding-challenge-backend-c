import React from "react"
import ReactDOM from "react-dom"
import { RecoilRoot, atom, selector, useRecoilState, useRecoilValue } from 'recoil'
import { SearchCities } from './searchCities'

import "./resources/scss/busbudcodechallenge.scss"

window.React = React;

const appContainer = document.querySelector('#busbudapp');

ReactDOM.render(
    <RecoilRoot>
        <SearchCities />
    </RecoilRoot>,
   appContainer 
);