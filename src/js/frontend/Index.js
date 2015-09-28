import React from "react";
import Main from "./Main";
import $ from "jquery";
import maps, { GoogleMap, Marker } from "react-google-maps";

export default class Index extends React.Component {
  constructor() {
    super();
    this.state = {
      query: "",
      suggestions: [],
      marker: null
    };
    this.timeout = null;
  }

  handleInput(event) {
    event.preventDefault();
    const query = React.findDOMNode(this.refs.search).value;
    if (this.timeout == null) {
      this.timeout = setTimeout(function () {
        this.timeout = null;
        this.performSearch();
      }.bind(this), 300);
    }

    this.setState({ query: query });
    this.forceUpdate();
  }

  handleSubmit(event) {
    event.preventDefault();
    this.performSearch();
  }

  performSearch() {
    if (this.state.query.length < 3) {
      this.setState({
        suggestions: []
      });
      this.forceUpdate();
      return;
    }

    const data = {
      q: this.state.query
    };

    if (this.state.marker != null) {
      data.latitude = this.state.marker.position.lat();
      data.longitude = this.state.marker.position.lng();
    }

    $.ajax("/suggestions", {
      type: "GET",
      dataType: "json",
      data: data,
      success: (response) => {
        this.setState({ suggestions: response.suggestions });
      },
      error: (xhr) => {
        this.setState({ suggestions: [] });
      }
    });
    return false;
  }

  /*
   * This is called when you click on the map.
   * Go and try click now.
   */
  _handle_map_click(event) {
    this.setState({
      marker: {
        position: event.latLng,
        defaultAnimation: 2,
        key: Date.now()// Add a key property for: http://fb.me/react-warning-keys
      }
    });
    this.forceUpdate();
    this.performSearch();
  }

  _handle_marker_rightclick(index, event) {
    this.setState({ marker: null });
    this.forceUpdate();
    this.performSearch();
  }

  render() {
    return (
      <Main>
        <div className="search-box">
          <p>Type in the box below to start searching. Click a location on the map to set your location--right click to remove.</p>
          <GoogleMap containerProps={{
              ...this.props,
              style: {
                height: "300px",
                width: "400px"
              },
            }}
                     ref="map"
                     defaultZoom={3}
                     defaultCenter={{lat: 42, lng: -95}}
                     onClick={this._handle_map_click.bind(this)}>
            {[this.state.marker].map((marker, index) => {
              return (
                <Marker
                  {...marker}
                  onRightclick={this._handle_marker_rightclick.bind(this, index)} />
              );
            })}
          </GoogleMap>
          <form id="search-form" onSubmit={this.handleSubmit.bind(this)}>
            <label for="search">Search for a city</label>
            <div><input onInput={this.handleInput.bind(this)} id="search" name="search" ref="search" value={this.state.query} autoComplete="off" autoCorrect="off" autoCapitalize="off" spellCheck="false" placeholder="Start typing the name of the city you would like to search for..." /></div>
          </form>
        </div>
        <div className="search-results">
          {this.state.suggestions.map((result) => {
            return (
              <div className="search-result">
                {result.display}
              </div>
            );
          })}
        </div>
      </Main>
    )
  }
}
