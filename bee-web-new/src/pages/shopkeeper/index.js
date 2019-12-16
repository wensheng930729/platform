import React, { Component } from 'react'
import { Button } from 'antd'
import { utils, go, openModal } from 'common'
import withRouter from 'umi/withRouter'
import '../../common/styles/layout/shopkeeper.less'
import '../../common/styles/theme/shopkeeper.less'

const { con } = go
const { imgGet } = utils
@withRouter
export default class Shopkeeper extends Component {
  constructor() {
    super()
    this.state = {
      erpIntro:
        '建立经营分析体系，发展精益成本管理，实时掌握经营各项成本构成，用数据把控业务风险，寻找突破空间，支撑管理决策',
      productInfo: [
        {
          name: '成本配置',
          iconSrc: 'cost_icon'
        },
        {
          name: '实时成本管控',
          iconSrc: 'time_cost_icon'
        },
        {
          name: '成本大数据 ',
          iconSrc: 'cost_data_icon'
        }
      ],
      enterpriseValue: [
        {
          title: '指标灵活配置',
          name: '—',
          iconSrc: 'resource_icon',
          imgSrc: 'product_1',
          abstract:
            '工厂根据自身情况灵活配置生产指标。科学预测原料采购性价比，提高关键元素回收率，模拟最优生产BOM'
        },
        {
          title: '降低产品成本',
          name: '—',
          iconSrc: 'optimization_icon',
          imgSrc: 'product_2',
          abstract:
            '结合原料指标及理化反应进行智能分析，优化料批。杜绝生产的过程浪费，提高单位产能，降低产品成本'
        },
        {
          title: '提高生产效率',
          name: '—',
          iconSrc: 'plan_icon',
          imgSrc: 'product_3',
          abstract:
            '智能的料批模拟系统，强化配料及下料管理，并监控相关指标变化，利于生产决策，提升工人效率'
        },
        {
          title: '提升产品竞争力',
          name: '—',
          iconSrc: 'process_icon',
          imgSrc: 'product_4',
          abstract:
            '通过蜂掌柜完善的成本管理体系，为企业压缩成本、提高产能的同时，强化质量管理，提升企业产品竞争力'
        }
      ]
    }
  }
  render() {
    const { erpIntro, productInfo, enterpriseValue } = this.state
    return (
      <div className="flex-column shopkeeperWrap">
        <div className="shopkBannerWrap">
          <div className="iconSize">
            <h2>蜂掌柜</h2>
            <h3>采购成本提前知晓，实现采购性价比最大化</h3>
            <h4>
              <span>成本配置 | 实时成本管控 | 成本大数据</span>
            </h4>
          </div>
          {/* <img className="iconSize" src={imgGet("banner", "shopkeeper")} /> */}
        </div>
        <div className="flex-column shopkIntroWrap">
          <div className="flex-column shopkIntroCont">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">—</span>蜂掌柜概述
                <span className="codeLine">—</span>
              </p>
              <p className="partnerName">Overview</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            <div className="shopkInfoText">{erpIntro}</div>
          </div>

          <div className="flex-column shopkFunctWrap">
            {productInfo && productInfo.length > 0 ? (
              <div className="flex-row shopkFunction">
                {productInfo.map((productItem, productIndex) => {
                  return (
                    <div
                      className="flex-column shopkFunctItem"
                      key={`functionItem${productIndex}`}
                    >
                      <p className="functionIcon">
                        <img
                          className="iconSize"
                          src={imgGet(productItem.iconSrc, 'shopkeeper')}
                        />
                      </p>
                      <p className="functionName">{productItem.name}</p>
                    </div>
                  )
                })}
              </div>
            ) : null}
          </div>
        </div>

        <div className="flex-column shopkValue">
          <div className="flex-column shopkValueContair">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">—</span>我们能为企业带来的价值
                <span className="codeLine">—</span>
              </p>
              <p className="partnerName">The value it brings</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            {enterpriseValue && enterpriseValue.length > 0 ? (
              <div className="flex-row shopkValueCont">
                {enterpriseValue.map((valueItem, valueIndex) => {
                  return (
                    <div className="shopkValueItem" key={`value${valueIndex}`}>
                      <p className="picImg">
                        <img
                          className="picImgSize"
                          src={imgGet(valueItem.imgSrc, 'shopkeeper')}
                        />
                      </p>
                      <p className="shopkCode">{valueItem.name}</p>
                      <p className="shopkItemName">{valueItem.title}</p>
                      <p className="shopkItemText">{valueItem.abstract}</p>
                    </div>
                  )
                })}
              </div>
            ) : null}
          </div>
        </div>
      </div>
    )
  }
}
