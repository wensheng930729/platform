import React, { Component } from 'react'
import { Divider, Icon, message } from 'antd';
import { connect } from 'dva';
import moment from 'moment';
import router from 'umi/router';
import Link from 'umi/link'
import { imgGet, getPageQuery } from '../../../common/utils'
import '../../../common/styles/newsDetail/index.less'
import { getPage } from '../services'
@connect(({ news }) => ({
  news,
}))
export default class Solution extends Component {
  state = {
    new: [],//最新活动
    fast: [],//金蜜快讯
    analysis: [],//分析评论
    page: null,
  }

  componentDidMount() {
    const { dispatch } = this.props;
    const { id } = this.props.location.query;
    const self = this;
    dispatch({
      type: 'news/getDetails',
      payload: id,
      success: () => {
        dispatch({
          type: 'news/getNewsList',
          payload: `type=0&size=5&page=1`,
          callback: (res1) => {
            dispatch({
              type: 'news/getNewsList',
              payload: `type=1&size=5&page=1`,
              callback: (res2) => {
                dispatch({
                  type: 'news/getNewsList',
                  payload: `type=2&size=5&page=1`,
                  callback: (res3) => {
                    self.setState({
                      news: res1.data.content,
                      fast: res2.data.content,
                      analysis: res3.data.content
                    })
                  }
                })
              }
            })
          }
        })
      },
      error: (msg) => {
        message.error("获取新闻详情失败：" + msg)
      }
    })
  }

  handleSkip = (id) => {
    // router.push(`/news/details?id=${id}`)
    const { dispatch } = this.props;
    dispatch({
      type: 'news/getDetails',
      payload: id,
      error: (msg) => {
        message.error("获取新闻详情失败：" + msg)
      }
    })
  }

  toNews = () => {
    router.push(`/news`)
  }

  getDetail = (id) => {
    console.log(id)
    const { dispatch } = this.props;
    const self = this
    dispatch({
      type: 'news/getDetails',
      payload: id,
      success: () => {

        // //获取上一篇下一篇数据
        // getPage(id).then(resp => {
        //   if (resp && resp.code === 0) {
        //     this.setState({
        //       page: resp.data
        //     })
        //   } else {
        //     message.warning('刷新失败，请重试')
        //   }
        // })
      },
      error: (msg) => {
        message.error(msg)
      }
    })
  }

  render() {
    const { details } = this.props.news;
    const { news, fast, analysis,page } = this.state;
    return (
      <div className='newsdetail-container'>
        <div className='head'></div>
        <div className='content'>
          <div className='breadcrumb'>
            <span>您的位置：<span onClick={() => { router.push('/news') }} style={{ cursor: 'pointer' }}>新闻中心</span> > </span><span>新闻详情</span>
          </div>
          <Divider style={{ margin: '30px 0' }} dashed />
          <div className='body'>
            <div className='left'>
              <p className='title'>{details.title ? details.title : '无'}</p>
              <div className='minorTitle'>
                <span>{details.createAt ? moment(details.createAt).format('YYYY-MM-DD') : '无'}</span>
                {/* <span><Icon type="switcher" style={{ fontSize: 18, color: '#FCBB01', marginRight: 20 }} />金蜜 / {
                  details.type ? (
                    details.type === 0 ? '最新活动' :
                      details.type === 1 ? '金蜜快讯' :
                        details.type === 2 ? '分析评论' :
                          details.type === 3 ? '人工智能' : '其他资讯'
                  ) : '无'
                }</span> */}
              </div>
              <div style={{fontSize:14}} className='main' dangerouslySetInnerHTML={{ __html: details.content }}></div>
              {/* <div className='pages'>
                <div onClick={this.getDetail.bind(this, page && page.id)} className='links'>上一篇：{page ? page.title : null}</div>
                <div onClick={this.getDetail.bind(this, page && page.id)} className='links'>下一篇：{page ? page.title : null}</div>
              </div> */}
            </div>

            <div className='right'>
              <div className='item'>
                <p className='heading'>
                  <img src={imgGet('huodongRed', 'news')} alt="news" />
                  <span>最新活动</span>
                </p>
                <div className='imgBox'>
                  {/* {
                    news && news.length !== 0 && news.map((item, index) => {
                      if (index < 2) {
                        return <div className='row' key={item.id} onClick={this.handleSkip.bind(this, item.id)}>
                          <div>
                            {
                              index === 0 ? <img src={imgGet('newsDefault0', 'news')} alt="" /> : <img src={imgGet('newsDefault1', 'news')} alt="" />
                            }
                          </div>
                          <p>{item.title ? item.title : ''}</p>
                        </div>
                      }
                    })
                  } */}
                </div>
                <div className='news'>
                  {
                    news && news.length !== 0 && news.map((item, index) => {
                      if (index > 1 && index < 5) {
                        return <div className='cl' key={item.id} onClick={this.handleSkip.bind(this, item.id)}>
                          <div></div>
                          <p><span></span>{item.title ? item.title : ''}</p>
                        </div>
                      }
                    })
                  }
                </div>
              </div>

              <div className='item'>
                <p className='heading'>
                  <Icon type="solution" style={{ fontSize: 22, color: '#FCBB01' }} />
                  <span>金蜜快讯</span>
                </p>
                <div className='imgBox'>
                  {/* {
                    fast && fast.length !== 0 && fast.map((item, index) => {
                      if (index < 2) {
                        return <div className='row' key={item.id} onClick={this.handleSkip.bind(this, item.id)}>
                          <div>
                            {
                              index === 0 ? <img src={imgGet('newsDefault2', 'news')} alt="" /> : <img src={imgGet('newsDefault3', 'news')} alt="" />
                            }
                          </div>
                          <p>{item.title ? item.title : ''}</p>
                        </div>
                      }
                    })
                  } */}
                </div>
                <div className='news'>
                  {
                    fast && fast.length !== 0 && fast.map((item, index) => {
                      if (index > 1 && index < 5) {
                        return <div className='cl' key={item.id} onClick={this.handleSkip.bind(this, item.id)}>
                          <div></div>
                          <p>{item.title ? item.title : ''}</p>
                        </div>
                      }
                    })
                  }
                </div>
              </div>

              <div className='item'>
                <p className='heading'>
                  <Icon type="message" style={{ fontSize: 22, color: '#FCBB01' }} />
                  <span style={{color:'black'}}>热门资讯</span>
                </p>
                {/* <div className='imgBox'>
                  {
                    analysis && analysis.length !== 0 && analysis.map((item, index) => {
                      if (index < 2) {
                        return <div className='row' key={item.id} onClick={this.handleSkip.bind(this, item.id)}>
                          <div>
                            {
                              index === 0 ? <img src={imgGet('newsDefault4', 'news')} alt="" /> : <img src={imgGet('newsDefault5', 'news')} alt="" />
                            }
                          </div>
                          <p>{item.title ? item.title : ''}</p>
                        </div>
                      }
                    })
                  }
                </div> */}
                <div className='news'>
                  {
                    news && news.length !== 0 && news.map((item, index) => {
                      if (index < 6) {
                        return <div className='cl' key={item.id} onClick={this.handleSkip.bind(this, item.id)}>
                          <div></div>
                          <p>{item.title ? item.title : ''}</p>
                        </div>
                      }
                    })
                  }
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    )
  }
}