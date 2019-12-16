import React, { Component } from 'react';
import styles from './index.less';
import { Breadcrumb, Divider, Radio, Input, Table, message } from 'antd';
import Link from 'umi/link';
import { domainUrl, path } from '../../utils/api';
import { get, post } from '../../utils/fetch';
import moment from 'moment';

const RadioGroup = Radio.Group;
const RadioButton = Radio.Button;
const Search = Input.Search;

const columns = [{
  title: '公司名称',
  dataIndex: 'name',
  key: 'name',
}, {
  title: '企业地址',
  dataIndex: 'address',
  key: 'address',
}, {
  title: '管理员帐号',
  dataIndex: 'admin',
  key: 'admin',
}, {
  title: '时间',
  dataIndex: 'createAt',
  key: 'createAt',
  render: text => moment(text).format('YYYY-MM-DD HH:mm:ss')
}, {
  title: '操作',
  key: 'action',
  render: (text, row, record) => (
    <Link to={`enterprise/details?id=${row.id}`}>查看详情</Link>
  ),
}];
export default class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      type: "1",
      counts: {},
      data: [],
      current: 1,
      pageSize: 10,
      currentSize: 0,
      total: 0,
      companyName: ''
    }
  }

  componentDidMount() {
    const { companyName } = this.state;
    get(path.enterpriseCount, domainUrl)
      .then(res => {
        if (res.code === 1 && res.object) {
          this.setState({
            counts: res.object
          })
        } else {
          message.error("获取企业管理数量失败")
        }
      })
    get(path.enterpriseUnhandleData + `?page=1&size=10&companyName=${companyName}`, domainUrl)
      .then(res => {
        if (res.code === 1 && res.object) {
          this.setState({
            data: res.object.content,
            currentSize: res.object.totalPages,
            total: res.object.totalElements
          })
        } else {
          message.error("获取未审核列表失败")
        }
      })
  }

  typeChange = (e) => {
    const { pageSize, companyName } = this.state;
    let url = '';
    if (e.target.value === '1') {
      url = path.enterpriseUnhandleData;
    } else if (e.target.value === '2') {
      url = path.enterprisePassedData;
    } else if (e.target.value === '3') {
      url = path.enterpriseRefusedData;
    }
    get(url + `?page=1&size=${pageSize}&companyName=${companyName}`, domainUrl)
      .then(res => {
        if (res.code === 1 && res.object) {
          this.setState({
            type: e.target.value,
            data: res.object.content,
            current: 1,
            currentSize: res.object.totalPages,
            total: res.object.totalElements,
          })
        } else {
          message.error("获取列表失败")
        }
      })
  }

  onChange = (current) => {
    const { type, pageSize, companyName } = this.state;
    let url = '';
    if (type === '1') {
      url = path.enterpriseUnhandleData;
    } else if (type === '2') {
      url = path.enterprisePassedData;
    } else if (type === '3') {
      url = path.enterpriseRefusedData;
    }
    get(url + `?page=${current}&size=${pageSize}&companyName=${companyName}`, domainUrl)
      .then(res => {
        if (res.code === 1 && res.object) {
          this.setState({
            data: res.object.content,
            currentSize: res.object.totalPages,
            total: res.object.totalElements,
            current: current
          })
        } else {
          message.error("获取列表失败")
        }
      })
  }

  onShowSizeChange = (current, size) => {
    const { type, companyName } = this.state;
    let url = '';
    if (type === '1') {
      url = path.enterpriseUnhandleData;
    } else if (type === '2') {
      url = path.enterprisePassedData;
    } else if (type === '3') {
      url = path.enterpriseRefusedData;
    }
    get(url + `?page=1&size=${size}&companyName=${companyName}`, domainUrl)
      .then(res => {
        if (res.code === 1 && res.object) {
          this.setState({
            data: res.object.content,
            current: 1,
            currentSize: res.object.totalPages,
            total: res.object.totalElements,
            pageSize: size
          })
        } else {
          message.error("获取列表失败")
        }
      })
  }

  handleSearch = value => {
    const { type, pageSize } = this.state;
    let url = '';
    if (type === '1') {
      url = path.enterpriseUnhandleData;
    } else if (type === '2') {
      url = path.enterprisePassedData;
    } else if (type === '3') {
      url = path.enterpriseRefusedData;
    }
    get(url + `?page=1&size=${pageSize}&companyName=${value}`, domainUrl)
      .then(res => {
        if (res.code === 1 && res.object) {
          this.setState({
            data: res.object.content,
            current: 1,
            currentSize: res.object.totalPages,
            total: res.object.totalElements,
            companyName: value
          })
        } else {
          message.error("获取列表失败")
        }
      })
  }

  render() {
    const { counts, data, current, pageSize, currentSize, total } = this.state;
    return (
      <div className={styles.container}>
        <div className={styles.crumb}>
          <Breadcrumb>
            <Breadcrumb.Item>企业管理</Breadcrumb.Item>
          </Breadcrumb>
        </div>

        {
          counts && <div className={styles.header}>
            <div className={styles.div}>
              <span>我的待办</span>
              <span>{counts.untreatedCount ? counts.untreatedCount : 0}</span>
            </div>
            <Divider type="vertical" style={{ height: '62px' }} />
            <div className={styles.div}>
              <span>已通过企业</span>
              <span>{counts.passedCount ? counts.passedCount : 0}</span>
            </div>
            <Divider type="vertical" style={{ height: '62px' }} />
            <div className={styles.div}>
              <span>申请企业总数</span>
              <span>{counts.sumCount ? counts.sumCount : 0}</span>
            </div>
          </div>
        }

        <div className={styles.body}>
          <div className={styles.top}>
            <span className={styles.title}>任务列表</span>
            <div>
              <RadioGroup defaultValue="1" onChange={this.typeChange.bind(this)}>
                <RadioButton value="1">未审核</RadioButton>
                <RadioButton value="2">已通过</RadioButton>
                <RadioButton value="3">未通过</RadioButton>
              </RadioGroup>
              <Search
                placeholder="请输入"
                onSearch={value => this.handleSearch(value)}
                style={{ width: 300, marginLeft: 20 }}
              />
            </div>
          </div>

          <Table
            style={{ marginTop: '30px' }}
            columns={columns}
            dataSource={data}
            rowKey={(i, index) => index}
            pagination={{
              showQuickJumper: true,
              showSizeChanger: true,
              defaultCurrent: 1,
              defaultPageSize: 10,
              current: current,
              pageSize: pageSize,
              total: total,
              onChange: this.onChange.bind(this),
              pageSizeOptions: ["10", "20", "30"],
              showTotal: (total, range) => `共 ${total} 条记录 第 ${current} / ${currentSize} 页`,
              onShowSizeChange: this.onShowSizeChange.bind(this)
            }}
          />
        </div>
      </div>
    )
  }
}
