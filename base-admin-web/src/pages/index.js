import React, { Component } from 'react';
import router from 'umi/router';

export default class Index extends Component {
  componentWillMount() {
    router.push('login')
  }

  render() {
    return "123";
  }
}