import React, { Component } from 'react';
import styles from './index.less';
import { Breadcrumb, Form, Button, Modal, Select, Input, Card, message, Icon, Avatar, Pagination,Empty  } from 'antd';
import Link from 'umi/link';
import { connect } from 'dva';
import moment from 'moment';
import router from 'umi/router';
import { getRoles, getTeam, getUserTeam, changeRole, getUserList } from './services';

const { Meta } = Card;
const Search = Input.Search;
const FormItem = Form.Item;
const Option = Select.Option

@Form.create()

export default class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentPage: 1,
      totalPage: 0,
      totalRecords: 0,
      params: {},
      visible: false,
      roleOptions: [],
      userData: {},
      showTeam: false,
      teamOptions: [],
      list: [],
      keyWord: ''
    }
    this.roleOptions = [];
    this.pageSize = 12
  }

  componentDidMount() {
    const params = { currentPage: 1, keyWord: '', pageSize: this.pageSize }
    getUserList(params).then(res => {

      if (res && res.code === 0) {
        this.setState({
          list: res.data,
          ...res.page
        })
      }
    })
  }

  handleSearch = (value) => {//搜索
    const params = { currentPage: 1, keyWord: value, pageSize: this.pageSize }
    getUserList(params).then(res => {
      if (res && res && res.code === 0) {
        this.setState({
          list: res.data,
          ...res.page
        })
      }
    })
    this.setState({
      keyWord: value
    })
  }

  showModal = (userData) => {//弹出修改框

    const { form } = this.props;
    userData.teamId = 1
    const roleIds = userData.roles && userData.roles.length ? userData.roles.map(item => item.roleId) : [];
    this.setState({
      visible: true,
      userData
    }, () => {
      form.setFieldsValue({ fullName: userData.fullName, userName: userData.userName });
      //获取角色选择
      if (this.state.roleOptions.length === 0) {
        getRoles().then(res => {
          this.roleOptions = res.data;//保存初始值
          this.setState({
            roleOptions: res.data
          }, () => {
            form.setFieldsValue({ roleIds });
            this.roleIdOnchang(roleIds);
            //获取部门选项
            this.setDefalutTeamId(userData)
          })
        })
      }
      else {
        form.setFieldsValue({ roleIds });
        this.roleIdOnchang(roleIds);
        //获取部门选项
        this.setDefalutTeamId(userData)
      }
    })
  }

  save = () => {//保存编辑
    const { form } = this.props;
    form.validateFields((err, values) => {
      if (!err) {
        
        const { roleIds, teamId } = values;
        let params = { roleIds, teamId };
        if(this.state.userData.userId>0){
          params.userId=this.state.userData.userId
        }else{
          params.userName=this.state.userData.userName
        }
        changeRole(params).then(res => {

          if (res && res.code === 0) {
            const params = { currentPage: this.state.currentPage, keyWord: this.state.keyWord, pageSize: this.pageSize }
            getUserList(params).then(resp => {

              if (resp.code === 0) {
                this.setState({
                  list: resp.data,
                  ...resp.page
                })
              }
            })
            message.success('修改成功');
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

  hideModal = () => {
    this.setState({
      visible: false
    })
  }

  //获取部门选项
  setDefalutTeamId = (userData) => {
    getUserTeam(userData.userId).then(team => {

      if (team.code === 0) {
        if (this.state.teamOptions.length === 0) {
          getTeam().then(res => {
            this.setState({ teamOptions: res.data }, () => {
              this.props.form.setFieldsValue({ teamId: team.data && team.data.teamId || '' })
            })
          })
        } else {
          this.props.form.setFieldsValue({ teamId: team.data && team.data.teamId || '' })
        }
      }
    })
  }

  //用户角色选择切换
  roleIdOnchang(roles) {
    let roleOptions = this.roleOptions;
    let showTeam = false;

    if (roles && roles.length > 0) {
      roles.map(id => {
        const checkedRole = roleOptions.filter(role => role.roleId === id);
        if (checkedRole[0].teamIn === 1) {
          showTeam = true
        }
        roleOptions = roleOptions.map(item => {
          if (item.permissionId === checkedRole[0].permissionId && item.roleId !== checkedRole[0].roleId) {
            return { ...item, disabled: true }
          }
          else {
            return { ...item }
          }
        })
      })
    } else {
      roleOptions = roleOptions.map(item => { return { ...item, disabled: false } })
    }
    this.setState({
      roleOptions,
      showTeam
    })
  }

  pageChange(currentPage) {
    const { keyWord } = this.state;
    const params = { currentPage: currentPage, keyWord: keyWord, pageSize: this.pageSize }
    getUserList(params).then(res => {
      if (res && res.code === 0) {
        this.setState({
          list: res.data,
          ...res.page
        })
      }
    })
  }

  showTotal(total, rangge) {
    return `${this.state.totalRecords}条记录  第${this.state.currentPage}/${this.state.totalPage}页`
  }

  render() {

    const { list, currentPage, totalPage, totalRecords } = this.state;
    const { roleOptions, userData, showTeam, teamOptions } = this.state;
    const { getFieldDecorator } = this.props.form;
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
    return (
      <div className={styles.container}>
        <div className={styles.crumb}>
          <span className={styles.title}>权限管理</span>
          <div className={styles.search}>
            <Search
              style={{ width: 600 }}
              placeholder="请输入要搜索的用户名或手机号"
              enterButton="搜索"
              size="large"
              onSearch={value => this.handleSearch(value)}
            />
          </div>
          <div className={styles.toManage}>

            <Button
              type='primary'
              onClick={() => { router.push('/supply/userManage/team') }}
            >管理团队</Button>
          </div>
        </div>
        <div className={styles.userPanel}>
          {list && list.length > 0 ? list.map(item => <Card
            style={{ width: 360, margin: 15 }}
            key={item.userName}
            actions={[<div onClick={() => this.showModal(item)}>修改</div>]}
          >
            <Meta
              avatar={<Avatar src={item.userPhoto} />}
              title={item.fullName}
              description={
                <div style={{ height: 80 }}>
                  <p>{item.userName}</p>
                  <p>{item.roles && item.roles.length > 0 && item.roles.map(role => `${role.roleName}　`)}</p>
                </div>
              }
            />
          </Card>
          ) : null}


        </div>
        <div className={styles.bottom}>
          {totalRecords > 0 ? <Pagination showQuickJumper style={{ display: 'inline-block' }}
            pageSize={this.pageSize} showTotal={this.showTotal.bind(this)} current={currentPage} total={totalRecords}
            onChange={this.pageChange.bind(this)} />:<Empty/>}
        </div>
        <Modal
          title="用户管理"
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
            <FormItem {...formItemLayout} label="姓名">
              {getFieldDecorator('fullName', {
                rules: [
                ]
              })(
                <Input
                  type="text"
                  disabled
                  placeholder="请输入姓名"
                />
              )}
            </FormItem>
            <FormItem {...formItemLayout} label="手机">
              {getFieldDecorator('userName', {
                rules: [
                ]
              })(
                <Input
                  type="phone"
                  disabled
                  placeholder="请输入手机号码"
                />
              )}
            </FormItem>
            <FormItem {...formItemLayout} label="角色">
              {getFieldDecorator('roleIds', {
                rules: []
              })(
                <Select
                  style={{ width: '100%' }}
                  placeholder="请选择角色"
                  onChange={this.roleIdOnchang.bind(this)}
                  mode='multiple'
                >
                  {roleOptions&&roleOptions.length > 0 ? roleOptions.map(item => (
                    <Option key={item.roleId} value={item.roleId} disabled={item.disabled}>
                      {item.roleName}
                    </Option>
                  )) : null}
                </Select>
              )}
            </FormItem>

            {showTeam ? <FormItem {...formItemLayout} label="综合营销部门">
              {getFieldDecorator('teamId', {
              })(
                <Select
                  style={{ width: '100%' }}
                  placeholder="请选择角色"
                >
                  {roleOptions&&teamOptions.length > 0 ? teamOptions.map(item => (
                    <Option key={item.teamId} value={item.teamId} >
                      {item.teamName}
                    </Option>
                  )) : null}
                </Select>
              )}
            </FormItem> : null}
          </Form>
        </Modal>

      </div>
    )
  }
}