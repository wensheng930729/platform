import React, { Component } from 'react';
import styles from './index.less';
import { Breadcrumb, Form, Button, Modal, Input, Select, Table, message, InputNumber } from 'antd';
import moment from 'moment';
import router from 'umi/router';
import { deleteLowRiskConfig, getSysCodeInfo, saveLowRiskConfig, getList } from './services/index'

const FormItem = Form.Item;
const Option = Select.Option;

@Form.create()

export default class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentPage: 1,
      pageSize: 10,
      totalPage: 0,
      totalRecords: 0,
      params: {},//搜索条件
      visible: false,
      goods: [],//标的物种类
      modifyId: null,//修改配置的id
      list: []
    }
  }

  componentDidMount() {
    this.getList()
  }

  getList(params = this.state.params, currentPage = this.state.currentPage, pageSize = this.state.pageSize) {
    getList(params, currentPage, pageSize).then(res => {
      if (res && res.code === 0) {
        this.setState({
          list: res.data,
          ...res.page
        })
      }
    })
  }

  onChange = (current) => {
    this.setState({
      currentPage: current
    }, () => this.getList())
  }
  //搜索條件改變時

  searchChange(type, value) {
    let { params } = this.state;
    params[type] = value;
    this.setState({ params });
  }

  onShowSizeChange = (current, size) => {
    this.setState({
      pageSize: size,
      currentPage: 1
    }, () => this.getList())
  }

  handleSearch = () => {
    this.getList()
  }

  handleReset = () => {
    this.setState({
      params: {}
    }, () => this.getList())
  }

  //删除
  handleDelete = (id, mode, e) => {
    const self = this;
    Modal.confirm({
      title: '规则删除',
      content: '您确定要删除该标的物吗？',
      okText: '确定',
      cancelText: '取消',
      onOk: () => {
        deleteLowRiskConfig({ id }).then((res) => {
          if (res.code === 0) {
            message.success('删除成功');
            this.getList()
          } else {
            message.error(res.msg)
          }

        })


      },
      onCancel: () => { }
    });
  }

  //配置或者新建
  toHandle(row) {//进入
    const { setFieldsValue } = this.props.form;
    if (this.state.goods.length === 0) {
      getSysCodeInfo().then(res => {
        if (res.code === 0 && res.data && res.data.length > 0) {
          this.setState({
            goods: res.data
          }
          )
        }
      })
    }
    if (row) {
      this.setState({
        modifyId: row.id,
        visible: true
      }, () => { setFieldsValue({ businessMode: row.businessMode + '', subjectName: row.subjectName + '', fractionalLine: row.fractionalLine + '' }) })
    } else {
      this.setState({
        visible: true,
        modifyId:null
      })
    }

  }

  hideModal = () => {
    this.setState({
      visible: false
    })
  }

  save = () => {//保存编辑
    const { form } = this.props;
    const { resetFields } = this.props.form;
    form.validateFields((err, values) => {
      if (!err) {
        let params = { ...values };
        if (this.state.modifyId) {
          params.id = this.state.modifyId
        }
        saveLowRiskConfig(params).then(res => {
          if (res.code === 0) {
            message.success(res.msg);
            this.handleSearch();
            resetFields();
            this.getList()
            this.setState({
              visible: false
            })
          } else {
            message.error(res.msg)
          }
        })
      }
    })
  }

  render() {

    const { list } = this.state;
    const { getFieldDecorator } = this.props.form;
    const { currentPage, pageSize, totalPage, totalRecords, modifyId, goods, params: { subjectName, businessMode } } = this.state;
    const formItemLayout = {
      labelCol: {
        xs: { span: 14 },
        sm: { span: 6 }
      },
      wrapperCol: {
        xs: { span: 34 },
        sm: { span: 18 }
      }
    }
    const columns = [{
      title: '业务类型',
      dataIndex: 'businessModeName',
      key: 'businessModeName',
    }, {
      title: '标的物品种',
      dataIndex: 'subjectName',
      key: 'name',
    }, {
      title: '分数线',
      dataIndex: 'fractionalLine',
      key: 'fractionalLine',
    }, {
      title: '操作',
      key: 'action',
      render: (text, row, record) => {
        let type = '';
        return (
          <div>
            <a onClick={() => this.toHandle(row)}>配置</a>
            <span style={{ color: '#1890ff', marginLeft: 20, cursor: 'pointer' }} onClick={this.handleDelete.bind(this, row.id, row.businessMode)}>删除</span>
          </div>
        )
      },
    }];

    return (
      <div className={styles.container}>
        <div className={styles.crumb}>
          <Breadcrumb>
            <Breadcrumb.Item>风控配置</Breadcrumb.Item>
          </Breadcrumb>
          <span className={styles.title}>风控配置</span>
        </div>

        <div className={styles.body}>

          <Form className={styles.top} layout='inline'>
            <FormItem label='标的物'>
              <Input style={{ width: 300 }} onChange={e => this.searchChange('subjectName', e.target.value)} value={subjectName} placeholder="请输入" />
            </FormItem>
            <FormItem label='业务类型'>
              <Select style={{ width: 300 }} placeholder="请选择" value={businessMode} onChange={this.searchChange.bind(this, 'businessMode')}>
                <Option value="0">委托采购</Option>
                <Option value="1">委托销售</Option>
                <Option value="4">大型企业委托采购</Option>
              </Select>
            </FormItem>
            <div className={styles.btnBox}>
              <Button type="primary" onClick={this.handleSearch.bind(this)}>查 询</Button>
              <Button style={{ marginLeft: 10 }} onClick={this.handleReset.bind(this)}>重 置</Button>

            </div>
          </Form>
          <div className={styles.add}> <Button type="primary" onClick={this.toHandle.bind(this, false)}>+ 新建</Button></div>
          <Table
            style={{ marginTop: '30px' }}
            columns={columns}
            dataSource={list}
            rowKey={(i, index) => index}
            pagination={{
              showQuickJumper: true,
              showSizeChanger: true,
              defaultCurrent: 1,
              defaultPageSize: 10,
              current: currentPage,
              pageSize: pageSize,
              total: totalRecords,
              onChange: this.onChange.bind(this),
              pageSizeOptions: ["10", "20", "30"],
              showTotal: (total, range) => `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
              onShowSizeChange: this.onShowSizeChange.bind(this)
            }}
          />
        </div>

        <Modal
          title={modifyId ? "修改风控配置" : '新建风控配置'}
          visible={this.state.visible}
          onOk={this.save}
          onCancel={this.hideModal}
          centered
          okText="确认"
          cancelText="取消"
        >
          <Form
            style={{ width: '465px', margin: '0 auto' }}
          >
            <FormItem {...formItemLayout} label="业务类型">
              {getFieldDecorator('businessMode', {
                rules: [{
                  required: true,
                  message: '请选择业务类型!',
                }]
              })(
                <Select placeholder="请选择业务类型">
                  <Option value='0'>委托采购</Option>
                  <Option value='1'>委托销售</Option>
                  <Option value="4">大型企业委托采购</Option>
                </Select>
              )}
            </FormItem>
            <FormItem {...formItemLayout} label="标的物">
              {getFieldDecorator('subjectName', {
                rules: [{
                  required: true,
                  message: '请选择标的物类型!',
                }]
              })(
                <Select
                  style={{ width: '100%' }}
                  placeholder="请选择标的物类型"
                >
                  {goods.length > 0 ? goods.map((item, index) => (
                    <Option key={item.sysCodeVal} value={item.sysCodeVal} >
                      {item.sysCodeVal}
                    </Option>
                  )) : null}
                </Select>
              )}
            </FormItem>
            <FormItem {...formItemLayout} label="分数线">
              {getFieldDecorator('fractionalLine', {
                rules: [
                  {
                    required: true,
                    message: '请填写分数线！',
                  }
                ]
              })(
                <InputNumber
                  style={{ width: '100%' }}
                  min={0}
                  max={100}
                  placeholder="请填写分数线"

                />

              )}
            </FormItem>
          </Form>
        </Modal>
      </div>
    )
  }
}