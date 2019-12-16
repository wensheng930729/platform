import React, { Component, Fragment } from 'react'
import style from './index.less'
import {
  Form,
  Icon,
  Input,
  Button,
  Checkbox,
  Row,
  Col,
  Spin,
  message,
  Divider
} from 'antd'
import { utils, apis, request } from 'common'
import withRouter from 'umi/withRouter'
import Link from 'umi/link'
import router from 'umi/router'
import { connect } from 'dva'

const { imgGet } = utils
const { Item } = Form

@withRouter
@connect(({ global }) => ({ global }))
class index extends Component {
  constructor(props) {
    super(props)
    this.state = {
      isShow: true,
      showTime: false,
      phoneChange: false,
      getCodeText: '获取验证码',
      time: null,
      step: 1,
      getCode: false
    }
    this.timer = null
  }
  componentDidMount() {
    document.addEventListener('keydown', this.handleNextKey.bind(this))
  }
  componentWillUnmount() {
    clearTimeout(this.timer)
    this.nextStep = null
    document.removeEventListener('keydown', this.handleNextKey)
  }
  handleNextKey = e => {
    const { step } = this.state
    if (e.which === 13) {
      this.nextStep(step)
    }
  }
  clickItem = show => {
    this.setState({
      isShow: show
    })
  }
  timeOut = second => {
    second = second - 1
    this.setState({
      time: second
    })
  }
  getRegistCode = () => {
    this.props.form.validateFields(['account'], (err, values) => {
      if (!err) {
        this.settime()
        request(apis.reset.getValidateCode.api() + `?phone=${values.account}`, {
          method: apis.reset.getValidateCode.type
          // headers: {
          //   'Content-Type': "application/x-www-form-urlencoded",
          //    subSysClientid: 'platform-v3.0',
          //   // 'sysToken': localStorage.sysToken
          // }
        }).then(res => {
          if (res && res.code === 0) {
            message.success(res.message)
          } else {
            res && message.error(res.message)
          }
        })
      }
    })
  }

  quit = sysToken => {
    const { dispatch } = this.props
    dispatch({
      type: 'global/userOut',
      payload: {
        sysToken: sysToken
      },
      callback: (code, msg, data) => {
        // if (code === 0) {
        //   this.getTime();
        //   router.push("/perLogin");
        // } else {
        //   // message.error(msg);
        // }
      }
    })
  }

  settime = () => {
    this.setState(
      {
        showTime: true
      },
      () => {
        if (countdown === 0) {
          this.setState({
            showTime: false,
            getCodeText: '重新获取'
          })
          countdown = 60
          return false
        } else {
          countdown--
          this.setState({
            time: countdown
          })
        }
        this.timer = setTimeout(() => {
          this.settime()
        }, 1000)
      }
    )
  }
  nextStep = step => {
    const {
      global: { user }
    } = this.props

    this.props.form.validateFields((err, values) => {
      if (!err) {
        if (step === 2) {
          request(
            apis.register.registCode.api() +
              `?${utils.queryString(values)}&type=0`,
            {
              method: apis.register.registCode.type
            }
          ).then(res => {
            if (res) {
              if (res.code === 0) {
                this.setState({
                  step: step,
                  phone: values.account,
                  ...values
                })
              } else {
                message.error(res.message)
              }
            }
          })
        } else if (step === 3) {
          if (values.password !== values.newPassword) {
            message.warning('两次密码输入不一致')
            return
          }
          request(
            apis.reset.resetPassword.api() +
              `?password=${values.password}&phone=${this.state.phone}`,
            {
              method: apis.reset.resetPassword.type
            }
          ).then(res => {
            if (res) {
              if (res.code === 0) {
                this.quit(user.sysToken)
                this.setState({
                  step: step
                })
              } else {
                message.warning(res.message)
              }
            }
          })
        }
      }
    })
  }

  handlePhoneChange = e => {
    const reg = /^1[3|4|5|6|7|8|9][0-9]{9}$/
    if (reg.test(e.target.value)) {
      this.setState({
        getCode: true
      })
    }
  }
  turnToLogin = () => {
    sessionStorage.originPath = location.pathname
    router.push('/perLogin')
  }
  render() {
    const {
      step,
      showTime,
      time,
      getCodeText,
      phoneChange,
      getCode
    } = this.state
    const { getFieldDecorator } = this.props.form
    const MyIcon = Icon.createFromIconfontCN({
      scriptUrl: '//at.alicdn.com/t/font_1093886_8rqycotthom.js'
    })
    return (
      <div className="getBackWrap">
        <div className="getBackBox">
          <div className="getBackContent">
            <h2>找回密码</h2>
            <div className="getBackHead">
              <dl>
                <dt style={active} />
                <dd
                  style={{
                    color: 'rgba(255, 206, 66, 1)'
                  }}
                >
                  验证身份
                </dd>
              </dl>
              <div
                className="processBar"
                style={step > 1 ? { background: 'rgba(255,206,66,1)' } : {}}
              />
              <dl>
                <dt style={step > 1 ? active : {}} />
                <dd style={step > 1 ? { color: 'rgba(255, 206, 66, 1)' } : {}}>
                  重置密码
                </dd>
              </dl>
              <div
                className="processBar"
                style={step > 2 ? { background: 'rgba(255,206,66,1)' } : {}}
              />
              <dl>
                <dt style={step > 2 ? active : {}} />
                <dd style={step > 2 ? { color: 'rgba(255, 206, 66, 1)' } : {}}>
                  重置成功
                </dd>
              </dl>
            </div>
            {step === 1 ? (
              <div className="getBackStepOne">
                <Form onSubmit={this.handleSubmit} className="login-form">
                  <Item className="inputBox">
                    {/* <span className="inputTitle">
                      <span className="letterDistance">账</span>
                      <span>号</span>
                    </span> */}
                    {getFieldDecorator('account', {
                      rules: [
                        {
                          required: true,
                          message: '手机号码格式不正确！',
                          pattern: /^1[3|4|5|6|7|8|9][0-9]{9}$/
                        }
                      ]
                    })(
                      <Input
                        onChange={this.handlePhoneChange}
                        addonBefore={<MyIcon type="icon-shouji" />}
                        // style={{ width: '510px' }}
                        placeholder="请输入手机号码"
                      />
                    )}
                  </Item>
                  <Item className="inputBox">
                    {/* <span className="inputTitle">验证码</span> */}
                    {getFieldDecorator('code', {
                      rules: [{ required: true, message: '请填入验证码' }]
                    })(
                      <Input
                        placeholder="请填入验证码"
                        addonBefore={
                          <Icon theme="filled" type="safety-certificate" />
                        }
                        addonAfter={
                          showTime ? (
                            <span
                              className="btnClass"
                              style={{
                                color: 'rgba(255,255,255,1)',
                                opacity: 0.5
                              }}
                            >
                              <Divider type="vertical" />
                              {time}s
                            </span>
                          ) : (
                            <span
                              className="btnClass"
                              style={
                                getCode
                                  ? { color: 'rgba(255,206,66,1)' }
                                  : {
                                      color: 'rgba(255,255,255,1)',
                                      opacity: 0.5
                                    }
                              }
                              onClick={getCode ? this.getRegistCode : () => {}}
                            >
                              <Divider type="vertical" />
                              获取验证码
                            </span>
                          )
                        }
                      />
                    )}
                  </Item>
                  <Item className="nextStep">
                    <Button
                      type="primary"
                      htmlType="submit"
                      onClick={this.nextStep.bind(this, 2)}
                    >
                      下一步
                    </Button>
                  </Item>
                </Form>
              </div>
            ) : null}
            {step === 2 ? (
              <div className="getBackStepTwo">
                <Form onSubmit={this.handleSubmit} className="login-form">
                  <Item className="inputBox">
                    {getFieldDecorator('password', {
                      rules: [
                        {
                          required: true,
                          message:
                            '请填入密码,密码为字母和数字的组合且不少于8位',
                          pattern: /^(?![0-9]+$)(?![a-zA-Z]+$)[0-9a-zA-Z]{8,}/
                        }
                      ]
                    })(
                      <Input
                        addonBefore={<Icon theme="filled" type="lock" />}
                        type="password"
                        placeholder="8-40位新密码，至少包含数字和字母"
                      />
                    )}
                  </Item>
                  <Item className="inputBox">
                    {getFieldDecorator('newPassword', {
                      rules: [
                        {
                          required: true,
                          message:
                            '请填入密码,密码为字母和数字的组合且不少于8位',
                          pattern: /^(?![0-9]+$)(?![a-zA-Z]+$)[0-9a-zA-Z]{8,}/
                        }
                      ]
                    })(
                      <Input
                        addonBefore={<Icon theme="filled" type="lock" />}
                        type="password"
                        placeholder="请再次输入新密码"
                      />
                    )}
                  </Item>
                  <Item className="nextStep">
                    <Button
                      type="primary"
                      htmlType="submit"
                      onClick={this.nextStep.bind(this, 3)}
                    >
                      下一步
                    </Button>
                  </Item>
                </Form>
              </div>
            ) : null}
            {step === 3 ? (
              <div className="getBackStepThree">
                <dl>
                  <dt>
                    <Icon type="check-circle" theme="filled" />
                  </dt>
                  <dd>重置成功</dd>
                </dl>
                <div>
                  <Button onClick={this.turnToLogin}>返 回 登 录</Button>
                </div>
              </div>
            ) : null}
          </div>
        </div>
      </div>
    )
  }
}
const active = {
  background: 'rgba(255, 206, 66, 1)',
  color: '#fff',
  border: '0'
}
let countdown = 60
const LoginForm = Form.create({})(index)

export default LoginForm
