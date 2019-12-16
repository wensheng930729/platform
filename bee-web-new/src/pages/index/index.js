import React, { Component } from 'react'
import { connect } from 'dva'
import withRouter from 'umi/withRouter'
import {
  Button,
  Row,
  Checkbox,
  Form,
  Input,
  Radio,
  Spin,
  message,
  Alert
} from 'antd'
// import { config } from 'utils';
import '../../common/styles/layout/index.less'
import Banner from './components/banner';
// import Banner from './components/newBanner'
import Product from './components/product'
import { Enterprise } from 'components'
const RadioGroup = Radio.Group
const FormItem = Form.Item

@connect(({ global, loading }) => ({
  global,
  logining: loading.effects['global/login']
}))
@Form.create()
class Login extends Component {
  constructor(props) {
    super(props)
    this.state = {}
  }
  // componentDidMount() {
  //   window.addEventListener('resize', this.handleResize.bind(this)) //监听窗口大小改变
  // }
  // componentWillUnmount() {
  //   window.removeEventListener('resize', this.handleResize.bind(this))
  // }
  // handleResize(){
  //   this.forceUpdate();
  // }

  handleOk() {
    const { form, dispatch } = this.props
    form.validateFieldsAndScroll((errors, values) => {
      if (errors) {
        return
      }
      if (!errors) {
        dispatch({
          type: 'global/login',
          payload: {
            ...values
          },
          callback: (status, msg) => status === 'error' && message.error(msg)
        })
      }
      // dispatch({ type: 'login/login', payload: values })
    })
  }

  renderMessage = content => {
    return (
      <Alert
        style={{ marginBottom: 24 }}
        message={content}
        type="error"
        closable
        showIcon
      />
    )
  }

  render() {
    return (
      <div className="flex-column pageWrap">
          <Banner />
          <Product />
          <Enterprise />

      </div>
    )
  }
}

export default withRouter(Login)
// Login.propTypes = {
//   form: PropTypes.object,
//   dispatch: PropTypes.func,
//   loading: PropTypes.object,
// }
