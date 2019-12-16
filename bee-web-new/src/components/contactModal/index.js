import React, { Component } from 'react'
import { Carousel, Button, Modal, Row, Col, Form, Input, message } from 'antd'
import { withRouter } from 'react-router'
import { connect } from 'react-redux'
import { config, request, apis, utils } from 'common'
import './index.less'

const { TextArea } = Input
const { imgGet, openModal } = utils

@withRouter
@connect(({ global, loading }) => ({
  global
}))
class Index extends Component {
  constructor(props) {
    super(props)
    this.state = {
      visable: false,
      nextVisible: false
    }
  }
  showModal = () => {
    this.setState({
      visible: true
    })
  }
  handleOk = () => {
    this.props.form.validateFields((err, values) => {
      if (!err) {
        request(`${config.ifms}${apis.addUser.addUserMsg.api}`, {
          method: apis.addUser.addUserMsg.type,
          body: JSON.stringify({ ...values }),
          headers: {
            'Content-Type': 'application/json'
          }
        }).then(res => {
          if (res.code === 202) {
            this.setState({
              nextVisible: true,
              visible: false
            })
            openModal()
          } else {
            message.error(res.message)
            openModal()
          }
        })
      }
    })
  }
  componentWillReceiveProps(nextProps) {
    if (nextProps.global.visible !== this.props.global.visible) {
      this.setState({
        visible: nextProps.global.visible
      })
    }
  }
  handleCancel = () => {
    this.setState({
      visible: false
    })
    openModal()
  }
  handleNextOk = () => {
    this.setState({
      nextVisible: false
    })
  }
  handleNextCancel = () => {
    this.setState({
      nextVisible: false
    })
  }
  render() {
    const { getFieldDecorator } = this.props.form
    const { visible } = this.state

    return (
      <div style={{ display: 'none' }} className="wrapper">
        <Modal
          className="sideResponseBox"
          title="咨询专家"
          okText="提交"
          visible={visible}
          footer={null}
          onOk={this.handleOk}
          onCancel={this.handleCancel}
        >
          <div className="form">
            <Form>
              <Form.Item>
                {getFieldDecorator('companyName', {
                  rules: [
                    {
                      required: true,
                      message: '请填写您所在的公司!'
                    }
                  ]
                })(
                  <Input
                    addonBefore={
                      <img
                        className="modalImg"
                        src={imgGet('yourCompany1', 'ifms')}
                      />
                    }
                    placeholder="您所在的公司"
                  />
                )}
              </Form.Item>
              <Form.Item>
                {getFieldDecorator('contactName', {
                  rules: [
                    {
                      type: 'string',
                      max: 6,
                      message: '您的姓名不得超过6字符!'
                    },
                    {
                      required: true,
                      message: '请填写您的姓名!'
                    }
                  ]
                })(
                  <Input
                    addonBefore={
                      <img className="modalImg" src={imgGet('name1', 'ifms')} />
                    }
                    placeholder="您的姓名"
                  />
                )}
              </Form.Item>

              <Form.Item>
                {getFieldDecorator('phone', {
                  rules: [
                    {
                      type: 'string',
                      max: 50,
                      message: '您的联系方式不得超过50字符!'
                    },
                    {
                      required: true,
                      message: '请填写您的联系方式!'
                    }
                  ]
                })(
                  <Input
                    addonBefore={
                      <img
                        className="modalImg"
                        src={imgGet('contactPhone1', 'ifms')}
                      />
                    }
                    placeholder="您的联系方式"
                  />
                )}
              </Form.Item>
              <Form.Item className="textArea">
                {getFieldDecorator('content', {
                  rules: [
                    {
                      type: 'string',
                      max: 200,
                      message: '疑问或需求不得超过200字符!'
                    },
                    {
                      // required: true,
                      message: '请输入您的电子邮箱！'
                    }
                  ]
                })(
                  <TextArea
                    autosize={{ minRows: 4, maxRows: 6 }}
                    placeholder={'您的疑问或需求'}
                  >
                    <img
                      className="modalImg"
                      src={imgGet('else', 'ifms')}
                      style={{ marginBottom: '120px' }}
                    />
                  </TextArea>
                )}
              </Form.Item>
              <Form.Item>
                <div className="btnClass">
                  <button onClick={this.handleCancel}>取消</button>
                  <button onClick={this.handleOk}>确认</button>
                </div>
              </Form.Item>
            </Form>
          </div>
        </Modal>
        <Modal
          className="nextResponseBox"
          title="申请已提交"
          okText="确定"
          visible={this.state.nextVisible}
          onOk={this.handleNextOk}
          footer={null}
          onCancel={this.handleNextCancel}
          closable={false}
        >
          <p className="modalText">
            温馨提示：我们已收到您的信息，客服会在1个工作日内和您取得联系。您也可以直接拨打客服电话{' '}
            <span className="modalPhone">028-85988267</span> 直接咨询。
          </p>
          <div className="nextDiv">
            <button onClick={this.handleNextOk}>确定</button>
          </div>
        </Modal>
      </div>
    )
  }
}

const contactModal = Form.create()(Index)
export default connect()(withRouter(contactModal))
