import React, { Component } from 'react';
import styles from '../index.less';
import { Breadcrumb, Divider, Form, Button, Modal, Input, Select, DatePicker, Table, message, Popconfirm } from 'antd';
import Link from 'umi/link';
import { connect } from 'dva';
import moment from 'moment';
import router from 'umi/router';
import { getTeamList, editTeam, addTeam, deleteTeam } from '../services';

const FormItem = Form.Item;
const Option = Select.Option;
const { RangePicker } = DatePicker;

@Form.create()

export default class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentPage: 1,
      pageSize: 10,
      totalPage: 0,
      totalRecords: 0,
      params: {},
      list: [],
      editDatas: {},//保存修改过的参数
    }
    this.defalutTeam = { "teamName": "", "notes": "", "managerName": "", "memberName": [], isEdit: true };
    this.newIndex = 1;//临时teamid索引值
  }

  componentDidMount() {
    getTeamList().then(res => {
      this.setState({ list: res.data })
    })
  }


  toHandle(index) {//编辑打开
    index = '' + index;
    let { list } = this.state;
    list = list.map(item => {

      if (item.teamId + '' === index) {
        return { ...item, isEdit: true }
      }
      return item
    })
    this.setState({ list });
  }

  editData(name, index, value) {//编辑数据操作
    index = '' + index;
    const { editDatas } = this.state;
    editDatas[`${name}${index}`] = value;
    this.setState({
      editDatas
    })
  }

  save(index, row) {//保存编辑或者添加保存
    index = '' + index;
    let params = {};
    params.notes = this.state.editDatas[`notes${index}`] || row.notes;
    params.teamName = this.state.editDatas[`teamName${index}`] || row.teamName;
    let { list } = this.state;

    if (index.indexOf('n') === -1) {
      params.teamId = index;
      editTeam(params).then(res => {

        if (res.code === 0) {
          message.success('编辑成功!');
          list = list.map(item => {

            if (item.teamId + '' === index) {
              return { ...item, notes: params.notes, teamName: params.teamName, isEdit: false }
            } else {
              return item
            }
          })
          this.setState({
            list
          })
        }
        else {
          message.error(res.msg)
        }
      })
    } else {
      addTeam(params).then(res => {

        if (res.code === 0) {
          message.success('添加成功!');
          list = list.map(item => {
            if (item.teamId + '' === index) {
              return { ...item, notes: params.notes, teamName: params.teamName, teamId: res.data, isEdit: false }
            } else {
              return item
            }
          })
          this.setState({
            list
          })
        } else {
          message.error(res.msg)
        }
      })
    }
  }

  addTeam() {//添加一列
    let { list } = this.state;
    list.push({ ...this.defalutTeam, teamId: `n${this.newIndex}` });
    this.newIndex++;
    this.setState({
      list
    })
  }

  deleteTeam(index) {//删除
    let params = {};
    params.teamId = index;
    let { list } = this.state;
    deleteTeam(params).then(res => {

      if (res.code === 0) {
        message.success('删除成功!');
        list = list.filter(item => item.teamId + '' !== index + '');
        this.setState({
          list
        })
      }
      else {
        message.error(res.msg)
      }
    })
  }

  cancel(index) {//取消编辑或者取消添加
    index = '' + index;
    let { list } = this.state;

    if (index.indexOf('n') === -1) {
      list = list.map(item => {
        if (item.teamId === Number(index)) {
          return { ...item, isEdit: false }
        } else {
          return item
        }
      })
    } else {
      list = list.filter(item => item.teamId !== index);
    }
    this.setState({ list })
  }

  render() {

    const { list, counts, editDatas } = this.state;
    const { getFieldDecorator } = this.props.form;
    const { currentPage, pageSize, totalPage, totalRecords } = this.state;
    const columns = [{
      title: '部门名称',
      dataIndex: 'teamName',
      key: 'entrustBuyId',
      width: 200,
      render: (text, row, index) => {
        if (row.isEdit) {
          if (editDatas[`teamName${row.teamId}`] !== undefined) {
            return <Input value={editDatas[`teamName${row.teamId}`]} onChange={e => this.editData('teamName', row.teamId, e.target.value)} />
          } else {
            this.editData('teamName', row.teamId, text)
          }
        } else {
          return <span>{text}</span>
        }
      }
    }, {
      title: '部门经理',
      dataIndex: 'managerName',
      key: 'name',
      width: 150,
    }, {
      title: '成员',
      dataIndex: 'memberName',
      key: 'mode',
      width: 300,
      render: (text, row) => {
        return <span>{text && text.length > 0 ? text.join(',') : '暂无'}</span>
      },
    }, {
      title: '备注',
      dataIndex: 'notes',
      key: 'status',
      width: 350,
      render: (text, row, index) => {
        if (row.isEdit) {
          if (editDatas[`notes${row.teamId}`] !== undefined) {
            return <Input value={editDatas[`notes${row.teamId}`]} onChange={e => this.editData('notes', row.teamId, e.target.value)} />
          } else {
            this.editData('notes', row.teamId, text)
          }
        } else {
          return <span>{text}</span>
        }
      }
    }, {
      title: '操作',
      key: 'action',
      render: (text, row, index) => {
        if (row.isEdit) {
          return <div>
            <a onClick={() => this.save(row.teamId, row)}>保存</a>
            <Popconfirm title="将放弃所有编辑？" onConfirm={this.cancel.bind(this, row.teamId)} okText="确定" cancelText="取消">
              <span style={{ color: '#1890ff', marginLeft: 20, cursor: 'pointer' }} >取消</span>
            </Popconfirm>
          </div>
        } else {
          return <div>
            <a onClick={() => this.toHandle(row.teamId)}>编辑</a>
            <Popconfirm title="确定删除？" onConfirm={this.deleteTeam.bind(this, row.teamId)} okText="确定" cancelText="取消">
              <span style={{ color: '#1890ff', marginLeft: 20, cursor: 'pointer' }}>删除</span>
            </Popconfirm>

          </div>
        }
      },
    }];

    return (
      <div className={styles.container}>
        <div className={styles.crumb}>
          <Breadcrumb>
            <Breadcrumb.Item>团队管理</Breadcrumb.Item>
          </Breadcrumb>
        </div>

        <div className={styles.body}>
          <span className={styles.title}>团队管理</span>
          <Table
            style={{ marginTop: '30px' }}
            columns={columns}
            dataSource={list}
            rowKey={(i, index) => index}
            pagination={false}
          />
          <div className={styles.add} onClick={this.addTeam.bind(this)}>添加</div>
        </div>
      </div>
    )
  }
}