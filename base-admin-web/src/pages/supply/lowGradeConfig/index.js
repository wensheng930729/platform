import React, { Component } from 'react';
import styles from './index.less';
import { Breadcrumb, Form, Button, Modal, Input, Select, Table, message } from 'antd';

import moment from 'moment';
import router from 'umi/router';
import { deleteLowGradeConfig, getList } from './services/index'

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
      params: { businessMode: '', subjectName: '' },
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

  searchChange(type, value) {
    let { params } = this.state;
    params[type] = value;
    this.setState({ params });
  }

  onChange = (current) => {
    this.setState({
      currentPage: current
    }, () => this.getList())
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
      params
    }, () => this.getList())
  }

  handleDelete = (id) => {
    const { dispatch } = this.props;
    const { pageSize, params } = this.state;
    const self = this;
    Modal.confirm({
      title: '规则删除',
      content: '您确定要删除该标的物吗？',
      okText: '确定',
      cancelText: '取消',
      onOk: () => {
        deleteLowGradeConfig({ lowConfigMasterIds: [id] }).then(() => {
          message.success('删除成功');
          this.getList()
        })
      },
      onCancel: () => { }
    });
  }

  //配置
  toHandle(id) {//进入
    const nextUrl = id === undefined ? `/supply/lowGradeConfig/configure` : `/supply/lowGradeConfig/configure?id=${id}`;
    router.push(nextUrl)
  }

  render() {

    const { list } = this.state;
    const { getFieldDecorator } = this.props.form;
    const { currentPage, pageSize, totalPage, totalRecords, params: { businessMode, subjectName } } = this.state;
    const columns = [{
      title: '业务类型',
      dataIndex: 'businessModeName',
      key: 'mode',
    }, {
      title: '标的物品种',
      dataIndex: 'subjectName',
      key: 'name',
    }, {
      title: '创建日期',
      dataIndex: 'createTime',
      key: 'time',
      sorter: (a, b) => moment(a.createTime) - moment(b.createTime),
    }, {
      title: '操作',
      key: 'action',
      render: (text, row, record) => {
        return (
          <div>
            <a onClick={() => this.toHandle(row.lowConfigMasterId)}>配置</a>
            <span style={{ color: '#1890ff', marginLeft: 20, cursor: 'pointer' }} onClick={this.handleDelete.bind(this, row.lowConfigMasterId, row.businessMode)}>删除</span>
          </div>
        )
      },
    }];

    return (
      <div className={styles.container}>
        <div className={styles.crumb}>
          <Breadcrumb>
            <Breadcrumb.Item>低分规则配置</Breadcrumb.Item>
          </Breadcrumb>
        </div>

        <div className={styles.body}>
          <span className={styles.title}>低分规则配置</span>
          <Form className={styles.top} layout='inline'>
            <FormItem label='标的物'>

              <Input style={{ width: 300 }} placeholder="请输入" value={subjectName} onChange={this.searchChange.bind(this, 'subjectName')} />
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
          <div className={styles.add}> <Button type="primary" onClick={() => this.toHandle()}>+ 新建</Button></div>



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
      </div>
    )
  }
}