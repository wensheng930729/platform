import React, { Component } from 'react'
import Content from "../components/Content"
import Modal from "../components/Modal"

export default class Purchase extends Component {
  constructor() {
    super()
    this.state = {
      visible: false,
      type: 0
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
        <Modal visible={this.state.visible} handleConsulting={this.handleConsulting} type={this.state.type} changeType={this.changeType}></Modal>
        <Content
          type="p"
          handleConsulting={this.handleConsulting}
        ></Content>
      </div>
    )
  }
}
