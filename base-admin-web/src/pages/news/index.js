import React, { Component } from 'react';
import styles from './index.less';
import { Breadcrumb, Divider, Form, DatePicker, Button, Input, Table, Modal, message, Select } from 'antd';
import Link from 'umi/link';
import { newsUrl, path, domainUrl } from '../../utils/api';
import { get, post, fetchPost } from '../../utils/fetch';
import moment from 'moment';
import router from 'umi/router';

const FormItem = Form.Item;
const { RangePicker } = DatePicker;
const Option = Select.Option;

@Form.create()
export default class index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      count: "0",
      data: [],
      current: 1,
      pageSize: 10,
      currentSize: 0,
      total: 0,
      type: null,
      params: {},
      selectedRowKeys: []
    }
  }

  componentDidMount() {
    get(path.newsCount, newsUrl)
      .then(res => {
        if ( res.code === 1 && res.object) {
          this.setState({
            count: res.object
          })
        } else {
          message.error("获取资讯总数失败")
        }
      })
    this.getData();
  }

  getCount() {

  }

  //獲取新聞
  getData(currentPage = this.state.current, type = this.state.type, pageSize = this.state.pageSize, params = this.state.params) {
    get(path.news + `?currentPage=${currentPage}&pageSize=${pageSize}${type === null ? '' : '&type=' + type}`, newsUrl, params)
      .then(res => {
        if ( res.code === 1 && res.object) {
          this.setState({
            data: res.object,
            current: res.page.currentPage,
            pageSize: res.page.pageSize,
            currentSize: res.page.totalPage,
            total: res.page.totalRecords,
            type,
            params
          })
        } else {
          message.error("获取资讯列表失败")
        }
      })
  }

  onChange = (current) => {
    this.getData(current);
  }

  onShowSizeChange = (page, size) => {
    this.getData(1, this.state.type, size);
  }

  handleQuery = () => {
    const { validateFields } = this.props.form;
    validateFields((err, data) => {
      if (err) {
        return;
      }
      let params = {};
      if (data.title) {
        params["title"] = data.title
      }
      if (data.type) {
        params["type"] = data.type
      }
      if (data.time) {
        params['startTime'] = moment(data.time[0]).format('YYYY-MM-DD')
        params['endTime'] = moment(data.time[1]).format('YYYY-MM-DD')
      }
      this.getData(1, this.state.type, this.state.pageSize, params);
    })
  }

  handleReset = () => {
    this.props.form.resetFields();
    this.getData(1, this.state.type, this.state.pageSize, {});
  }

  handleDelete = (id) => {
    let param = {
      newIds: []
    };
    if (id) {
      param.newIds = [id]
    } else {
      if (this.state.selectedRowKeys.length === 0) {
        return message.warning("请先选择至少一条资讯");
      }
      param.newIds = this.state.selectedRowKeys;
    }
    post(path.newsDelete, domainUrl, param)
      .then(res => {
        if ( res.code === 1 && res.object) {
          message.success("删除资讯成功");
          get(path.newsCount, newsUrl)
            .then(res => {
              if ( res.code === 1 && res.object) {
                this.setState({
                  count: res.object
                })
              } else {
                message.error("获取资讯总数失败")
              }
            })
          this.getData(1, this.state.type, this.state.pageSize);
        } else {
          message.error("删除资讯失败");
        }
      })
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { count, data, current, pageSize, currentSize, total } = this.state;
    const columns = [{
      title: '标题',
      dataIndex: 'title',
      key: 'title',
    }, {
      title: '发布时间',
      dataIndex: 'createAt',
      key: 'createAt',
      render: text => moment(text).format('YYYY-MM-DD HH:mm:ss')
    }, {
      title: '操作',
      key: 'action',
      render: (text, row, record) => (
        <span>
          <a style={{ marginRight: 10 }} onClick={this.handleDelete.bind(this, row.id)}>删除</a>
          <Link to={`news/details?id=${row.id}`}>查看详情</Link>
        </span>
      ),
    }];
    const rowSelection = {
      onChange: (selectedRowKeys, selectedRows) => {
        let arr = [];
        for (let i = 0; i < selectedRows.length; i++) {
          arr.push(selectedRows[i].id)
        }
        this.setState({
          selectedRowKeys: arr
        })
      }
    };

    return (
      <div className={styles.container}>
        <div className={styles.crumb}>
          <Breadcrumb>
            <Breadcrumb.Item>资讯管理</Breadcrumb.Item>
          </Breadcrumb>
        </div>

        <div className={styles.header}>
          <div className={styles.div}>
          </div>
          <Divider type="vertical" style={{ height: '62px' }} />
          <div className={styles.div}>
            <span>资讯总数</span>
            <span>{count}</span>
          </div>
          <Divider type="vertical" style={{ height: '62px' }} />
          <div className={styles.div}>
          </div>
        </div>

        <div className={styles.body}>
          <Form className={styles.formBox}>
            <FormItem label="标题" className={styles.formItem}>
              {getFieldDecorator('title')(
                <Input style={{ width: 200 }} />
              )}
            </FormItem>
            <FormItem label="类型" className={styles.formItem}>
              {getFieldDecorator('type')(
                <Select style={{ width: 200 }}>
                  <Option value="0">头条资讯</Option>
                  <Option value="1">金蜜快讯</Option>
                  <Option value="2">分析评论</Option>
                  <Option value="3">人工智能</Option>
                  <Option value="4">其他资讯</Option>
                </Select>
              )}
            </FormItem>
            <FormItem label="选择日期" className={styles.formItem}>
              {getFieldDecorator('time', { rules: [{ type: 'array' }] })(
                <RangePicker />
              )}
            </FormItem>
            <div>
              <Button type="primary" onClick={this.handleQuery.bind(this)}>查询</Button>
              <Button style={{ marginLeft: 20 }} onClick={this.handleReset.bind(this)}>重置</Button>
            </div>
          </Form>
          <div style={{ marginTop: 30 }}>
            <Button type="primary" icon="plus" onClick={() => router.push('news/edit')}>新建</Button>
            <Button style={{ marginLeft: 20 }} onClick={this.handleDelete.bind(this, null)}>删除</Button>
          </div>

          <Table
            rowKey={(i, index) => i.id}
            style={{ marginTop: '30px' }}
            rowSelection={rowSelection}
            columns={columns}
            dataSource={data}
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