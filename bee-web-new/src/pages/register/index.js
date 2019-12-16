import React, { Component } from 'react'
import style from './index.less'
import {
  Form,
  Input,
  Button,
  Row,
  Col,
  Tooltip,
  Icon,
  Checkbox,
  Radio,
  message,
  Divider
} from 'antd'
import Link from 'umi/link'
import apis from '@/common/api'
import request from '@/common/request'
import { imgGet } from 'common/utils'
import { utils } from '@/common'

let countdown = 60
const formDates = datas => {
  let result = new FormData()
  for (const i in datas) {
    result.append(i, datas[i])
  }
  return result
}
@Form.create()
export default class Index extends Component {
  constructor() {
    super()
    this.state = {
      showTime: false,
      time: null,
      getCode: false
    }
    this.registerTimer = null
  }
  componentDidMount() {
    document.addEventListener('keydown', this.handleRegisterKey.bind(this))
  }
  componentWillUnmount() {
    clearTimeout(this.registerTimer)
    document.removeEventListener('keydown', this.handleRegisterKey)
  }
  handleRegisterKey = e => {
    if (e.which === 13) {
      this.handleSubmit()
    }
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
        this.registerTimer = setTimeout(() => {
          this.settime()
        }, 1000)
      }
    )
  }
  getRegistCode = () => {
    const self = this
    const { validateFields, getFieldsValue, getFieldValue } = this.props.form
    validateFields(['phone'], (err, values) => {
      if (!err) {
        request(apis.register.getRegistCode.api() + `?phone=${values.phone}`, {
          method: apis.register.getRegistCode.type
          // headers: {
          //   'Content-Type': "application/x-www-form-urlencoded",
          //    subSysClientid: 'platform-v3.0',
          //   // 'sysToken': localStorage.sysToken
          // }
        }).then(res => {
          if (res && res.code === 0) {
            self.settime()
            message.success(res.message)
          } else {
            res && message.error(res.message)
          }
        })
      }
    })
  }

  handleSubmit = e => {
    const self = this
    e && e.stopPropagation()
    const { validateFields, getFieldsValue, getFieldValue } = this.props.form
    const value = getFieldsValue()
    if (!value.agreement) {
      if(location.pathname==='/register'){
        message.warning('注册时请先阅读并同意《金密工业云用户注册协议》')
      }
      return
    }
    const code = value.code
    const password = value.password
    const rePassWord = value.rePassWord
    if (password !== rePassWord) {
      // validateFields(["rePassWord"])
      message.warning('两次密码输入不一致')
      return
    }
    if (!value.code) {
      validateFields(['code'])
      return
    }
    const { dispatch, type } = this.props
    validateFields((err, values) => {
      if (!err) {
        request(
          apis.register.registCode.api() +
            `?code=${code}&account=${value.phone}&type=0`,
          {
            method: apis.register.registCode.type
            // body: {
            //   code,
            //   phone: value.phone
            // },
            // headers: {
            //   'Content-Type': "application/x-www-form-urlencoded"
            // }
          }
        ).then(resp => {
          if (resp.code === 0) {
            delete values.rePassWord
            delete values.agreement
            delete values.code
            request(
              apis.register.regist.api() + `?${utils.queryString(values)}`,
              {
                method: apis.register.regist.type
              }
            ).then(response => {
              if (response) {
                if (response.code === 0) {
                  message.success(response.message)
                  sessionStorage.originPath = location.pathname
                  self.props.history.push('/perLogin')
                } else {
                  message.error(response.message)
                }
              }
            })
          } else {
            message.error(resp.message)
          }
        })
      }
    })
  }

  handlePhoneChange = e => {
    const reg = /^1[3|4|5|6|7|8|9][0-9]{9}$/
    const { getCode } = this.state
    if (reg.test(e.target.value)) {
      this.setState({
        getCode: true
      })
    } else if (getCode) {
      this.setState({
        getCode: false
      })
    }
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const { showTime, time, getCode } = this.state
    const MyIcon = Icon.createFromIconfontCN({
      scriptUrl: '//at.alicdn.com/t/font_1093886_8rqycotthom.js'
    })
    const MyIconTwo = Icon.createFromIconfontCN({
      scriptUrl: '//at.alicdn.com/t/font_1093886_6s5qjyg1qs.js'
    })
    return (
      <div className="registerWrap">
        <div className="content">
          {/* <div className="banner}>
            <img src="https://obs-fe91.obs.cn-south-1.myhuaweicloud.com/87075157d514484c9797741b30f10013.png" alt=""/>
          </div> */}
          <div className="registerBox">
            <div className="leftBox_wrap">
              <h2>为工业赋能，与伙伴共生</h2>
              <p>立足数字化转型，助力企业高效运营</p>
            </div>
            <div className="form">
              <h3>用户注册</h3>
              <Form>
                <Form.Item>
                  {getFieldDecorator('phone', {
                    rules: [
                      {
                        required: true,
                        message: '手机号码格式不正确！',
                        pattern: /^1[3|4|5|6|7|8|9][0-9]{9}$/
                      }
                    ]
                  })(
                    <Input
                      placeholder="请输入手机号码"
                      onChange={this.handlePhoneChange}
                      addonBefore={<MyIcon type="icon-shouji" />}
                    />
                  )}
                </Form.Item>
                <Form.Item>
                  <Row className="row">
                    <Col span={24}>
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
                                onClick={
                                  getCode ? this.getRegistCode : () => {}
                                }
                              >
                                <Divider type="vertical" />
                                获取验证码
                              </span>
                            )
                          }
                        />
                      )}
                    </Col>
                  </Row>
                </Form.Item>
                <Form.Item>
                  {getFieldDecorator('name', {
                    rules: [
                      {
                        required: true,
                        message: '请填入用户名！'
                      }
                    ]
                  })(
                    <Input
                      placeholder="请填入用户名"
                      addonBefore={<MyIcon type="icon-user" />}
                    />
                  )}
                </Form.Item>
                <Form.Item>
                  {getFieldDecorator('password', {
                    rules: [
                      {
                        required: true,
                        message: '请填入密码,密码为字母和数字的组合且不少于8位',
                        pattern: /^(?![0-9]+$)(?![a-zA-Z]+$)[0-9a-zA-Z]{8,}/
                      }
                    ]
                  })(
                    <Input
                      type="password"
                      placeholder="请填入密码"
                      addonBefore={<Icon theme="filled" type="lock" />}
                    />
                  )}
                </Form.Item>
                <Form.Item>
                  {getFieldDecorator('rePassWord', {
                    rules: [
                      {
                        required: true,
                        message:
                          '请再次输入密码,密码为字母和数字的组合且不少于8位',
                        whitespace: true
                      }
                    ]
                  })(
                    <Input
                      placeholder="请再次输入密码"
                      type="password"
                      addonBefore={<Icon theme="filled" type="lock" />}
                    />
                  )}
                </Form.Item>

                <Form.Item style={{ textAlign: 'center' }}>
                  {getFieldDecorator('agreement', {
                    valuePropName: 'checked'
                  })(
                    <Checkbox className="radio">
                      同意并阅读
                      <Link
                        to="/agreement/regment"
                        style={{ color: 'rgba(252, 187, 1, 1)' }}
                      >
                        《金蜜工业云用户注册协议》
                      </Link>
                    </Checkbox>
                  )}
                </Form.Item>
                <Form.Item style={{ textAlign: 'center' }}>
                  <Button
                    type="primary"
                    className="registerBtn"
                    onClick={this.handleSubmit}
                  >
                    立 即 注 册
                  </Button>
                </Form.Item>
              </Form>
            </div>
          </div>
        </div>
      </div>
    )
  }
}
