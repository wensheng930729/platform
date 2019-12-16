import React, { Component } from 'react';
import styles from '../index.less';
import { Breadcrumb, Form, Button, Input, Select, message, Popconfirm } from 'antd';
import router from 'umi/router';
import Link from 'umi/link';
import { connect } from 'dva';
import withRouter from 'umi/withRouter';
import { findUpdateCloumns, findLowGradeConfig, saveLowGradeConfig, getTriggerType } from '../services'
import { getSysCodeInfo } from '../../lowRiskConfig/services'

const FormItem = Form.Item;
const Option = Select.Option;
@withRouter

export default class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentPage: 1,
      pageSize: 10,
      totalPage: 0,
      totalRecords: 0,
      cloumns: [],//低分规则配置可修改字段
      cloumnsData: null,//低分规则配置值
      triggerTypes: [],//低分规则的风控评分类型
      data: { businessMode: 0, subjectName: '' },//标的物与业务类型值
      goods: [],//所有标的物
    }
    this.id = this.props.location.query.id || false
  }

  componentDidMount() {
    // 初始值
    this.getSysCodeInfo();
    const { id } = this.props.location.query;
    if (id) {
      this.findLowGradeConfig(id, (businessMode) => { this.findUpdateCloumns(businessMode, true); });
    } else {
      this.findUpdateCloumns()
    }
  }

  //获得标的物
  getSysCodeInfo() {
    getSysCodeInfo('HP').then(res => {
      if (res.code === 0 && res.data && res.data.length > 0) {
        this.setState({
          goods: res.data
        })
      }
    })

  }

  //查询低分规则配置可修改字段
  findUpdateCloumns(businessMode = 0, init = false) {
    findUpdateCloumns(businessMode).then(res => {
      if (res.code === 0) {
        if (this.state.cloumnsData && init) {
          this.setState({
            cloumns: res.data,
          })
        } else {
          this.setState({
            cloumns: res.data,
            cloumnsData: initCloumnDate(res.data)
          })

        }
      } else {
        this.setState({
          cloumns: []
        })
      }
      getTriggerType(businessMode).then(result => {
        if (result.code === 0) {
          this.setState({
            triggerTypes: result.data
          })
        } else {
          this.setState({
            triggerTypes: []
          })
        }
      })

    })
  }

  //查询低分规则配置可修改字段
  getTriggerType(businessMode = 0, callback) {
    getTriggerType(businessMode).then(res => {
      if (res.code === 0) {
        this.setState({
          cloumns: res.data
        }, () => { callback && callback() })
      } else {
        this.setState({
          cloumns: []
        })
      }
    })
  }

  //标的物低分规则配置详细查询
  findLowGradeConfig(id, callback) {
    findLowGradeConfig(id).then(res => {
      if (res.code === 0) {
        let cloumnsData = {};
        if (res.data.updateColumnInfo && res.data.updateColumnInfo.length) {
          cloumnsData = handleCloumnDate(res.data.updateColumnInfo)
        }
        this.setState({
          cloumnsData,
          data: { businessMode: res.data.businessMode, subjectName: res.data.subjectName }
        }, () => { callback && callback(res.data.businessMode) })
      } else {
        this.setState({
          cloumns: []
        })
      }
    })
  }

  //标的物与业务类型值更改
  valueChange(type, value) {
    let data = this.state.data;
    data[type] = value;
    this.setState({ data });

    if (type === 'businessMode') {
      this.findUpdateCloumns(value)
    }
  }

  //配置项值更改
  cloumnsChange(id, type, value) {
    let { cloumnsData } = this.state;
    cloumnsData[`data${id}`][type] = value;
    this.setState({ cloumnsData })
  }

  save() {
    const { cloumnsData, data: { businessMode, subjectName } } = this.state;
    let columnRQList = [];
    for (let key in cloumnsData) {
      columnRQList.push(cloumnsData[key])
    }
    let params = { columnRQList, businessMode, subjectName };
    if (this.props.location.query.id) {
      params.lowConfigMasterId = this.props.location.query.id
    }
    saveLowGradeConfig(params).then(res => {
      if (res.code === 0) {
        message.success(res.msg, 1.5, () => { router.replace('/supply/lowGradeConfig') })
      }
      else {
        message.error(res.msg)
      }
    })
  }

  render() {
    const { cloumns, cloumnsData, triggerTypes, data: { businessMode, subjectName }, goods } = this.state;
    return (
      <div className={styles.container}>
        <div className={styles.crumb}>
          <Breadcrumb className={styles.breadcrumb}>
            <Breadcrumb.Item><Link to={'/supply/lowGradeConfig'}>规则配置</Link></Breadcrumb.Item>
            <Breadcrumb.Item>{subjectName}</Breadcrumb.Item>
          </Breadcrumb>
          <div className={styles.title}>规则配置详情</div>
          <div className={styles.btnBox}>
            <Popconfirm title="退出后内容将不能保存" okText="确定" cancelText="取消" onConfirm={() => { router.push('/supply/lowGradeConfig') }}><Button type="primary" size='large'>取 消</Button></Popconfirm>
            <Button style={{ marginLeft: 10 }} size='large' onClick={this.save.bind(this)}>保 存</Button>
          </div>
          <Form layout='inline'>
            <FormItem label='业务类型'>
              <Select disabled={this.id ? true : false} style={{ width: 300 }} onChange={(value) => this.valueChange('businessMode', value)} placeholder="请选择" value={businessMode}>
                <Option value={0}>委托采购</Option>
                <Option value={1}>委托销售</Option>
                <Option value={4}>大型企业委托采购</Option>
              </Select>
            </FormItem>
            <FormItem label='标的物类型'>
              <Select
                style={{ width: 300 }}
                placeholder="请选择标的物类型"
                onChange={(e) => this.valueChange('subjectName', e)}
                value={subjectName || []}
              >
                {goods && goods.length > 0 ? goods.map(item => (
                  <Option key={item.sysCodeVal} value={item.sysCodeVal} >
                    {item.sysCodeVal}
                  </Option>
                )) : null}
              </Select>
            </FormItem>

          </Form>
        </div>
        <div className={styles.body}>
          <span className={styles.title}>低分规则配置</span>
          <Form layout='inline'>
            {cloumns && cloumns.length > 0 && cloumns.map(item => <div className={styles.top} key={item.id}>
              <FormItem>
                <Select style={{ width: 150 }} placeholder="请选择" value={cloumnsData[`data${item.id}`].triggerType} onChange={value => this.cloumnsChange(item.id, 'triggerType', value)}>
                  {triggerTypes && triggerTypes.length > 0 && triggerTypes.map(trigger => <Option key={trigger.sysCode} value={trigger.sysCode}>{trigger.sysCodeDesc}</Option>)}
                </Select>
              </FormItem>
              分数低于 &nbsp;
              <FormItem>
                <Input style={{ width: 100 }} value={cloumnsData[`data${item.id}`].triggerGrade} onChange={e => this.cloumnsChange(item.id, 'triggerGrade', e.target.value)} />
              </FormItem>
              分，则<span style={{ minWidth: 150, fontWeight: 600 }}>{item.updateColumnName}</span>强制改为&nbsp;
            {item.options ?
                <FormItem>
                  <Select style={{ width: 250 }} placeholder="请选择" value={cloumnsData[`data${item.id}`].updateContent} onChange={value => this.cloumnsChange(item.id, 'updateContent', value)}>
                    {item.options.map(trigger => <Option key={trigger.key} value={trigger.key + ''}>{trigger.desc}</Option>)}
                  </Select>
                </FormItem>
                : <Input style={{ width: 250 }} value={cloumnsData[`data${item.id}`].updateContent} onChange={e => this.cloumnsChange(item.id, 'updateContent', e.target.value)} />}
              &nbsp;<span>{item.unit}</span>
            </div>)}
          </Form>
        </div>

      </div>
    )
  }
}

//初始化规则配置的值
const initCloumnDate = (data = []) => {
  let obj = {};
  data.forEach(item => {
    obj[`data${item.id}`] = { triggerGrade: '', triggerType: '', updateColumnId: item.id, updateContent: '' }
  })
  return obj;
}

//后台传回的值处理成为规则配置的值
const handleCloumnDate = (data = []) => {
  let obj = {};
  data.forEach(item => {
    obj[`data${item.updateColumnId}`] = { triggerGrade: item.triggerGrade, triggerType: item.triggerType !== undefined ? item.triggerType + '' : '', updateColumnId: item.updateColumnId, updateContent: item.updateContent }
  })
  return obj;
}