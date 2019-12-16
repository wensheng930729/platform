import React, { Component } from 'react'
import { connect } from 'dva'
import Link from 'umi/link'
// import styles from './index.less'
import '../../common/styles/news/index.less'
import { Pagination } from 'antd'
import { imgGet } from '../../common/utils'
import moment from 'moment'

@connect(({ news }) => ({
  news
}))
export default class Index extends Component {
  constructor(props) {
    super(props)
    this.state = {
      type: 1,
      news: [],
      newsList: [],
      currentPage: 1,
      pageSize: 6,
      totalPages: 1,
      totalRecords: 0,
      value: 0
    }
  }

  componentDidMount() {
    window.addEventListener('scroll', this.handleScroll)
    this.getDatas()
    console.log(Math.random() * 10)
  }

  componentWillUnmount() {
    window.removeEventListener('scroll', this.handleScroll)
  }

  // handleScroll = event => {
  //   const { dispatch } = this.props
  //   //滚动条高度
  //   let top = document.documentElement.scrollTop || document.body.scrollTop;
  //   let { value, currentPage, totalPages, newsList, type, pageSize } = this.state
  //   let clientHeight =
  //     document.documentElement.clientHeight || document.body.clientHeight; //可视区域高度
  //   let scrollHeight =
  //     document.documentElement.scrollHeight || document.body.scrollHeight; //滚动内容高度
  //   if (top - value >= 600 && currentPage < totalPages) {//如果距离上次刷新后又滚动了至少800PX，并且不是最后一页
  //     value = top;

  //     // Fetch.get(`/beetrade-business/product/getSaleInfoListForPlatform`, params)
  //     // .then(resp => {
  //     //   if (resp && resp.code === 0) {
  //     //     this.setState({
  //     //       newsList: detail.concat(resp.data),
  //     //       value,
  //     //       currentPage:currentPage + 1
  //     //     })
  //     //   }
  //     // })

  //     dispatch({
  //       type: 'news/getNewsList',
  //       payload: `type=${type}&size=${pageSize}&page=${currentPage + 1}`,
  //       callback: (resp) => {
  //         this.setState({
  //           newsList: newsList.concat(resp.data.content),
  //           currentPage: currentPage + 1,
  //           value,
  //         })
  //       }
  //     });
  //   }
  // }

  //导航切换
  navChanged(type) {
    // debugger
    this.getDatas(type, 1)
    this.setState({
      type,
      currentPage: 1,
      value: 0
    })
  }

  //请求数据
  getDatas = (type = this.state.type, currentPage = this.state.currentPage) => {
    const { dispatch } = this.props
    const { pageSize } = this.state
    dispatch({
      type: 'news/getNewsList',
      payload: `type=${type}&size=${pageSize}&page=${currentPage}`,
      callback: resp => {
        // console.log(resp)
        this.setState({
          newsList: resp.data.content,
          currentPage: resp.data.number,
          pageSize: resp.data.size,
          totalPages: resp.data.totalPages,
          totalRecords: resp.data.totalElements
        })
      }
    })
  }

  //暂未使用
  pageChange(currentPage) {
    this.getDatas(undefined, currentPage)
    this.setState({
      currentPage
    })
  }

  onChange = currentPage => {
    const { type } = this.state
    this.getDatas(type, currentPage)
    this.setState({
      currentPage
    })
  }

  //提取富文本中文字
  getSimpleText = html => {
    let re1 = new RegExp('<.+?>', 'g') //匹配html标签的正则表达式，"g"是搜索匹配多个符合的内容
    let msg = html.replace(re1, '') //执行替换成空字符
    let reg = new RegExp('&nbsp;', 'g') //替换空格
    let str = msg.replace(reg, '')
    return str
  }

  handleDate = time => {
    if (!time) {
      time = moment(new Date().format('YYYY-MM-DD hh:mm:ss'))
    }
    let arr
    arr = time.slice(0, 10).split('-')
    return arr
  }

  renderImg = () => {
    const num = Math.random() * 10
    if (num > 0 && num < 2) {
      return imgGet('newsDefault0', 'news')
    } else if (num > 2 && num < 4) {
      return imgGet('newsDefault1', 'news')
    } else if (num > 4 && num < 6) {
      return imgGet('newsDefault2', 'news')
    } else if (num > 6 && num < 8) {
      return imgGet('newsDefault3', 'news')
    } else if (num > 8 && num < 10) {
      return imgGet('newsDefault4', 'news')
    }
  }

  render() {
    const navList = [
      {
        key: 1,
        title: '金蜜快讯',
        icon: 'kuaixun',
        iconAc: 'kuaixunAc'
      },
      {
        key: 0,
        title: '最新活动',
        icon: 'huodong',
        iconAc: 'huodongAc'
      },
      {
        key: 2,
        title: '分析评论',
        icon: 'pinlun',
        iconAc: 'pinlunAc'
      },
      {
        key: 3,
        title: '人工智能',
        icon: 'zhineng',
        iconAc: 'zhinengAc'
      },
      {
        key: 4,
        title: '其它资讯',
        icon: 'zixun',
        iconAc: 'zixunAc'
      }
    ]
    const {
      type,
      currentPage,
      pageSize,
      totalPages,
      totalRecords,
      total,
      newsList
    } = this.state
    // const { newsList } = this.props.news
    return (
      <div className="news-main">
        {/* <Banner type={1} /> */}
        <div className="banner">
          <div></div>
          {/* <img style={{width:'100%',height:640}} src='https://obs-fe91.obs.cn-south-1.myhuaweicloud.com/%2Fvar0a59e27e8d7644888730481b4eaa15f9.jpg' /> */}
        </div>
        <div className="body">
          <div className="bodyWrapper">
            <div className="bodyLeft">
              <div className="xinwentu"></div>
              <ul className="nav" style={{ width: 158 }}>
                {navList.map(item => (
                  <li
                    className={type === item.key ? 'navItemAc' : 'navItem'}
                    key={item.key}
                    onClick={this.navChanged.bind(this, item.key)}
                  >
                    <img
                      src={
                        type === item.key
                          ? imgGet(item.iconAc, 'news')
                          : imgGet(item.icon, 'news')
                      }
                      style={{ marginRight: 6, width: 20, height: 18 }}
                    />
                    <span style={{ marginLeft: 10 }}>{item.title}</span>
                  </li>
                ))}
              </ul>
            </div>
            <div className="bodyRight">
              <div className="rightHead">
                {/* <img
                  src={imgGet('sanjiao', 'news')}
                  style={{ marginRight: 10 }} />
                <span className='title'>{
                  type === 0 ? '最新活动' :
                    type === 1 ? '金蜜快讯' :
                      type === 2 ? '分析评论' :
                        type === 3 ? '人工智能' :
                          '其它资讯'
                }</span> */}
              </div>
              <div className="contentWrapper">
                {newsList && newsList.length > 0
                  ? newsList.map((item, index) => {
                      if (1) {
                        //如果有图，对应排版
                        return (
                          <div
                            className="contentImg"
                            key={index}
                            style={{ fontSize: 18 }}
                          >
                            <div className="left">
                              <div className="zuo">
                                <p
                                  style={{
                                    fontSize: 20,
                                    color: 'white',
                                    paddingLeft: 7
                                  }}
                                >
                                  {this.handleDate(item.createAt)[2]}
                                </p>
                                <p style={{ fontSize: 12, color: 'white' }}>
                                  {this.handleDate(item.createAt)[0]}.
                                  {this.handleDate(item.createAt)[1]}
                                </p>
                              </div>
                              <div className="you">
                                {/* <p style={{fontSize:20,marginBottom:10,color:'white'}}>2019即时物流行业报告发布</p> */}
                                <Link
                                  to={`/news/details?id=${item.id}`}
                                  className="link"
                                  style={{
                                    fontSize: 20,
                                    marginBottom: 10,
                                    color: 'white',
                                    display: 'block'
                                  }}
                                >
                                  {item.title}
                                </Link>
                                <p
                                  className="desc"
                                  style={{
                                    fontSize: 14,
                                    color: 'rgba(158,161,164,1)',
                                    overflow: 'hidden',
                                    height: 40
                                  }}
                                >
                                  {this.getSimpleText(item.content)}
                                </p>
                              </div>
                            </div>
                            <div className="right">
                              <img
                                src={item.img || this.renderImg()}
                                style={{ width: 193, height: 110 }}
                              />
                            </div>
                          </div>
                        )
                      } else {
                        //没图，对应排版
                        return (
                          <div className="content" key={index}>
                            <div className="title" style={{ fontSize: 18 }}>
                              <Link
                                to={`/news/details?id=${item.id}`}
                                className="link"
                                style={{ fontSize: 18 }}
                              >
                                {item.title}
                              </Link>
                            </div>
                            <div className="desc" style={{ fontSize: 18 }}>
                              {this.getSimpleText(item.content)}
                            </div>
                            <div className="mark" style={{ fontSize: 18 }}>
                              <div className="markLeft">
                                <span className="markIcon"></span>
                                <span style={{ marginLeft: 10 }}>金蜜</span>
                                <span
                                  style={{ marginLeft: 10, marginRight: 10 }}
                                >
                                  /
                                </span>
                                <span>科技</span>
                              </div>
                              <div className="markRight">
                                {moment(item.createAt).format('YYYY-MM-DD')}
                              </div>
                            </div>
                          </div>
                        )
                      }
                    })
                  : null}
                <div>
                  {/* <Pagination
                    showQuickJumper
                    defaultCurrent={currentPage}
                    current={currentPage}
                    defaultPageSize={pageSize}
                    total={total}
                    onChange={this.onChange}
                  /> */}

                  <Pagination
                    // showQuickJumper
                    // defaultCurrent={currentPage}
                    // current={currentPage}
                    // defaultPageSize={pageSize}
                    // total={total}
                    // onChange={this.onChange}
                    style={{ marginTop: 80, color: 'white', marginBottom: 260 }}
                    showQuickJumper={true}
                    // showSizeChanger={true}
                    defaultCurrent={1}
                    defaultPageSize={10}
                    current={currentPage}
                    pageSize={pageSize}
                    total={totalRecords}
                    onChange={this.onChange.bind(this)}
                    // pageSizeOptions={["10", "20", "30"]}
                    showTotal={(total, range) =>
                      `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPages} 页`
                    }
                    // onShowSizeChange={this.onShowSizeChange.bind(this)}
                  />
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    )
  }
}

//重写toLocaleString
// Date.prototype.toLocaleString = function () {
//   let month = this.getMonth() + 1
//   let day = this.getDate()
//   if (month < 10) month = '0' + month
//   if (day < 10) day = '0' + day
//   return this.getFullYear() + "-" + month + "-" + day;
// };
