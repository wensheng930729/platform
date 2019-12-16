import React, { Component } from 'react'
import { Modal, Form, Input, Select, Button, message } from 'antd'
import './Modal.less'
import { connect } from 'dva'
import { moneyExp } from '@/common/utils'

const { Option } = Select

@Form.create()
@connect(({ supplyChain, global }) => ({
  supplyChain,
  global
}))
export default class ApplyModal extends Component {
  constructor(props) {
    super(props)
    this.state = {
      visible: false,
      tradeGoods: []
    }
  }
  componentDidMount() {
    const { dispatch } = this.props
    this.setState({
      applyCompanyName: localStorage.companyName,
      userToken: localStorage.token,
      applyCompanyId: localStorage.companyId
    })
    // dispatch({
    //   type: "supplyChain/getSubjectMatterList",
    //   callback(res) {
    //     if (res.code === 0) {
    //       this.setState({
    //         tradeGoods: res.data
    //       });
    //     } else {
    //       message.error(res.msg);
    //     }
    //   }
    // })
  }

  componentWillReceiveProps(props) {
    this.setState({
      visible: props.visible
      // type: props.type
    })
  }
  // static getDerivedStateFromProps(nextProps, prevState) {
  //   return {
  //     visible: nextProps.visible
  //   }
  // }

  selectChange = e => {
    this.setState({
      type: e
    })
  }

  closeBox = () => {
    this.setState(
      {
        visible: false
      },
      () => {
        this.props.backChange()
      }
    )
  }

  handleSubmit = e => {
    const self = this
    e.stopPropagation()
    const { validateFields } = this.props.form
    const { dispatch } = this.props
    const { type } = this.state
    validateFields((err, values) => {
      if (!err) {
        const { applyCompanyName, applyCompanyId } = this.state
        const params = {
          applyCompanyName: applyCompanyName,
          applyCompanyId: applyCompanyId,
          tradeGoodsTypeName: values.tradeGoodsTypeName,
          sysToken: localStorage.getItem('sysToken')
        }
        const method =
          type === 0
            ? 'postBuyApplication'
            : type === 1
            ? 'postSaleApplication'
            : type === 2
            ? 'postStorageApplication'
            : 'postLargeBuyApplication'
        dispatch({
          type: 'supplyChain/' + method,
          payload: {
            ...params,
            ...values
          },
          callback(res) {
            if (res.code === 1) {
              message.success('申请已提交，请等待工作人员与你联系！')
              self.props.backChange()
            } else {
              message.error(res.msg)
            }
            // if (res.code === 0) {
            //   message.success('申请已提交，请等待工作人员与你联系！');
            //   self.setState(
            //     {
            //       visible: false
            //     }
            //   );
            // } else {
            //   message.error(res.msg);
            // }
          }
        })
      }
    })
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const {
      type,
      global: { user }
    } = this.props
    const { tradeGoods } = this.state
    const formItemLayout = {
      labelCol: {
        xs: { span: 24 },
        sm: { span: 6 }
      },
      wrapperCol: {
        xs: { span: 24 },
        sm: { span: 16 }
      }
    }
    const tailFormItemLayout = {
      wrapperCol: {
        xs: {
          span: 24,
          offset: 0
        },
        sm: {
          span: 16,
          offset: 8
        }
      }
    }
    const button = {
      background: 'url(../../assets/supply/9.png) no-repeat center center',
      backgroundColor: '#ffce42',
      boxShadow: '0px 1px 30px 0px rgba(60,59,136,0.1)',
      borderRadius: '2px',
      width: 150,
      height: 40,
      border: 'none',
      color: '#000116'
    }
    return (
      <Modal
        title="服务申请表"
        visible={this.state.visible}
        onOk={this.props.handleOk}
        onCancel={this.props.handleCancel}
        style={{ width: 548, height: 531 }}
        wrapClassName="modal"
        footer={null}
        closable={false}
      >
        <div className={'closeBox'} onClick={this.closeBox}>
          X
        </div>
        <div className="form">
          <Form {...formItemLayout}>
            <Form.Item label="申请企业">
              <span style={{ color: '#fff' }}>
                {user.org_name ? user.org_name : ''}
              </span>
            </Form.Item>
            <Form.Item label="业务模式">
              {getFieldDecorator('businessMode', {
                initialValue: type,
                rules: [
                  {
                    required: true,
                    message: '请选择业务模式'
                  }
                ]
              })(
                <Select
                  onChange={this.selectChange}
                  placeholder="请选择"
                  style={{ height: 56 }}
                >
                  <Option value={0}>委托采购</Option>
                  <Option value={1}>委托销售</Option>
                  <Option value={2}>金融仓储</Option>
                  <Option value={4}>大企业委托采购</Option>
                </Select>
              )}
            </Form.Item>
            {this.state.type === 2 ? (
              ''
            ) : (
              <Form.Item label="上游/下游企业">
                {getFieldDecorator('upDownstreamName', {
                  rules: [
                    {
                      type: 'string',
                      message: '请填入上游/下游企业，不超过20个字符！',
                      required: true,
                      max: 20
                    }
                  ]
                })(<Input placeholder="请填入上游/下游企业" />)}
              </Form.Item>
            )}

            <Form.Item label="交易货品">
              {getFieldDecorator('tradeGoodsTypeName', {
                rules: [
                  {
                    required: true,
                    message: '请填写交易货品！'
                  }
                ]
              })(<Input placeholder="请填入货品" />)}
            </Form.Item>
            <Form.Item label="数量">
              {getFieldDecorator('quantity', {
                rules: [
                  {
                    required: true,
                    message: '货品数量填入不正确！',
                    pattern: new RegExp(/^[+]{0,1}(\d+)$|^[+]{0,1}(\d+\.\d+)$/g)
                  }
                ]
              })(<Input placeholder="请填入数量" />)}
            </Form.Item>
            <Form.Item label="预计金额">
              {getFieldDecorator('estimatedAmount', {
                rules: [
                  {
                    required: true,
                    message: '请填入预计金额！',
                    pattren: moneyExp
                  }
                ]
              })(<Input placeholder="请输入预计金额" />)}
            </Form.Item>
            <Form.Item
              label=""
              wrapperCol={{
                xs: { span: 24 },
                sm: { span: 24 }
              }}
              style={{}}
            >
              <Button
                type="primary"
                style={{ float: 'right', marginRight: '8%', ...button }}
                onClick={this.handleSubmit}
              >
                确定
              </Button>
            </Form.Item>
          </Form>
        </div>
      </Modal>
    )
  }
}

ApplyModal.defaultProps = {
  visible: true,
  handleOk() {},
  handleCancel() {}
}
