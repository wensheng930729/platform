import React, { Component } from 'react'
import style from './Content.less'
import { openModal } from '@/common/utils'
import { connect } from 'dva'
import { HeaderNav } from 'components'

//供应链子页面组件
//接受props
// type (业务类型) user (用户信息)
@connect(({ supplyChain, global }) => ({
  supplyChain,
  global
}))
export default class Content extends Component {
  constructor() {
    super()
  }

  // componentDidMount() {
  //   const { dispatch } = this.props
  //   dispatch({
  //     type: "supplyChain/"
  //   })
  //   api/user/getSelfInfo
  // }

  //按钮处理函数
  handleConsulting = () => {
    if (!localStorage.companyName) {
      openModal()
      return
    }
    this.props.handleConsulting()
  }

  render() {
    const {
      type,
      global: { user }
    } = this.props
    const navMsg = [
      {
        key: 'supplyChain',
        navTitle: '首页',
        link: '/supplyChain'
      },
      {
        key: 'purchase',
        navTitle: '委托采购',
        link: '/supplyChain/purchase'
      },
      {
        key: 'sell',
        navTitle: '委托销售',
        link: '/supplyChain/sell'
      },
      {
        key: 'storage',
        navTitle: '金融仓储',
        link: '/supplyChain/storage'
      }
    ]
    return (
      <div className={style.contentWrap}>
        <HeaderNav
          navMsg={navMsg}
          imgMsg={{
            path: 'supply',
            imgName: 21
          }}
          color={'rgba(208,134,68,1)'}
          // borderColor="rgba(208,134,68,1)"
        />
        <div className={style.bannerWrap}>
          <div className={style.banner}>
            <img
              src={
                type === 'p'
                  ? 'https://obs-fe91.obs.cn-south-1.myhuaweicloud.com/c63c9ae229df4651937e3ff4f1a63c1c.png'
                  : type === 's'
                  ? 'https://obs-fe91.obs.cn-south-1.myhuaweicloud.com/fb45d4e3448242bea695f62db51cf71f.png'
                  : type === 'w'
                  ? 'https://obs-fe91.obs.cn-south-1.myhuaweicloud.com/cbb7c9d89acc4ac4bc8f2a37c555a526.png'
                  : ''
              }
              alt=""
            />
            <div className={style.bannerWrapTwo}>
              <div className={style.title}>
                <p>
                  {type === 'p'
                    ? '委托采购'
                    : type === 's'
                    ? '委托销售'
                    : type === 'w'
                    ? '金融仓储'
                    : ''}
                </p>
                <h1>
                  {type === 'p'
                    ? '信用赋能，扩大上游供货商销售规模。委托企业只需指定上游供货商并提供保证，便可在采购的过程中享受领蜂供应链提供的“第三方采购”服务，实现实时付款提货。'
                    : type === 's'
                    ? '提高双方交易效率，稳定下游购货渠道。当企业的销售面临回款压力时，可通过领蜂供应链提供的“定向销售”服务出售您的货权，让销售货款即刻回笼。'
                    : type === 'w'
                    ? '激发存货活力，破解融资瓶颈。融资企业将货物存放在领蜂供应链指定的仓库并进行监管，并将货物质押给由领蜂供应链合作的资金方，资金方按照货物价值的一定比例打折后放款。'
                    : ''}
                </h1>
                <div className={style.box}>
                  <button onClick={this.handleConsulting}>
                    {localStorage.companyName ? '立即委托' : '咨询专家'}
                  </button>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div className={style.content}>
          <div className={style.top}>
            <ul>
              <li style={{ marginRight: 19 }}>
                <div className={style.imgBox}>
                  <img src={img(5)} alt="" />
                  <span />
                </div>
                <div className={style.right}>
                  <h4>目标客户</h4>
                  <p>
                    {type === 's'
                      ? '有销售意向、信用意识过关、履约能力较强的卖方。'
                      : type === 'w'
                      ? '希望将库存质押给资金方，获取资金用于生产经营。'
                      : type === 'p'
                      ? '有采购意向、信用意识过关、履约能力较强的买方。'
                      : ''}
                  </p>
                </div>
              </li>
              <li style={{ marginRight: 19 }}>
                <div className={style.imgBox}>
                  <img src={img(6)} alt="" />
                </div>
                <div className={style.right}>
                  <h4>定价方式</h4>
                  <p>
                    {type === 's'
                      ? '根据市场价格灵活、合理定价。'
                      : type === 'w'
                      ? '领蜂供应链根据委托企业实际占用资金情况收取服务费。'
                      : type === 'p'
                      ? '根据市场价格灵活、合理定价。'
                      : ''}
                  </p>
                </div>
              </li>
              <li>
                <div className={style.imgBox}>
                  <img src={img(7)} alt="" />
                </div>
                <div className={style.right}>
                  <h4>服务优势</h4>
                  <p>
                    {type === 's'
                      ? '快速回款、拓宽销售渠道、扩大销售规模。'
                      : type === 'w'
                      ? '支持多种货物，高效审核效率，及时解决委托企业资金难题。'
                      : type === 'p'
                      ? '降本增效，缩减委托企业的采购周期。'
                      : ''}
                  </p>
                </div>
              </li>
            </ul>
            <div className={style.flow}>
              <h2>流程介绍</h2>
              <img src={img(3)} alt="" className={style.imgOne} />
              <img
                src={
                  type === 's'
                    ? img('sflow')
                    : type === 'w'
                    ? img('wflow')
                    : type === 'p'
                    ? img('pflow')
                    : ''
                }
                alt=""
                className={style.imgTwo}
              />
            </div>
          </div>
        </div>
      </div>
    )
  }
}

Content.defaultProps = {
  user: {}
}

const img = id => {
  return require('assets/supply/' + id + '.png')
}
