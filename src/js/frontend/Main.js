import React from "react";
import Header from "./Header";

export default class Main extends React.Component {
  render() {
    return (
      <div className="main-content">
        <Header />
        <main>
          {this.props.children}
        </main>
      </div>
    )
  }
}
