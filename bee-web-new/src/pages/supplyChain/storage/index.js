import React, { Component } from 'react'
import Content from "../components/Content"
import Modal from "../components/Modal"

export default class Storage extends Component {
  constructor() {
    super()
    this.state = {
      visible: false,
      type: 2
    }
  }

  changeType = (e) => {
    this.setState({
      type: e
    })
  }

  handleConsulting = () => {
    this.setState({
      visible: !this.state.visible
    })
  }
  render() {
    return (
      <div>
        <Modal visible={this.state.visible} handleConsulting={this.handleConsulting} type={this.state.type} storage changeType={this.changeType}></Modal>
        <Content
          type="w"
          handleConsulting={this.handleConsulting}
        ></Content>
      </div>
    )
  }
}
