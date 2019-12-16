import React, { Component } from 'react';
import styles from './index.less';
import { Breadcrumb, Icon, Divider, message } from 'antd';
import Link from 'umi/link';
import { newsUrl, path } from '../../../utils/api';
import { get, post, fetchPost } from '../../../utils/fetch';
import moment from 'moment';

export default class index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      data: {},
      name: ''
    }
  }

  componentDidMount() {
    const { query } = this.props.location;
    get(path.newsDetails + `/${query.id}`, newsUrl)
    .then(res => {
      if (res.code === 1 && res.object) {
        this.setState({
          data: res.object.news,
          name: res.object.username
        })
      } else {
        message.error("获取资讯详情失败")
      }
    })
  }

  render() {
    const { data, name } = this.state;
    return (
      <div className={styles.container}>
        <div className={styles.crumb}>
          <Breadcrumb>
            <Breadcrumb.Item><Link to='/news'>资讯管理</Link></Breadcrumb.Item>
            <Breadcrumb.Item>资讯详情</Breadcrumb.Item>
          </Breadcrumb>
          <span className={styles.name}>{data && data.title ? data.title : '无'}</span>
        </div>

        <div className={styles.body}>
          <p className={styles.title}>{data && data.title ? data.title : '无'}</p>
          <div className={styles.top}>
            <div className={styles.item}>
              <Icon type="eye" />
              <span style={{marginLeft: 8}}>{data && data.hits ? data.hits : '0'}</span>
            </div>
            <div className={styles.item}>
              <Icon type="clock-circle" />
              <span style={{marginLeft: 8}}>{data && data.createAt ? moment(data.createAt).format('YYYY-MM-DD HH:mm:ss') : '无'}</span>
            </div>
            <div className={styles.item}>
              <Icon type="user" />
              <span style={{marginLeft: 8}}>{name ? name : '无'}</span>
            </div>
            <div className={styles.item}>
              <Icon type="edit" style={{color: "#1890ff"}}/>
              <Link to={`/news/edit?id=${data.id}`} style={{marginLeft: 8}}>编辑</Link>
            </div>
          </div>
          <Divider style={{width: 720, minWidth: 0}}/>
          <div className={styles.htmlBox} dangerouslySetInnerHTML = {{ __html: data.content}}></div>
        </div>
      </div>
    )
  }
}