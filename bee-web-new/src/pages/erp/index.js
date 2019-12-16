import React, { Component } from 'react'
import { Button } from 'antd'
import { utils, go, openModal } from 'common'
import withRouter from 'umi/withRouter'
import '../../common/styles/layout/erp.less'
import '../../common/styles/theme/erp.less'

const { con } = go
const { imgGet } = utils
@withRouter
export default class Erp extends Component {
  constructor() {
    super()
    this.state = {
      erpIntro:
        '汇集全区域、全产品线、全业务、全系统数据，形成基础数据集成平台，实现产供销一体化，通过信息化手段节约供应链成本',
      productInfo: [
        {
          name: '采购管理',
          iconSrc: 'purchasing_icon'
        },
        {
          name: '销售管理',
          iconSrc: 'sale_icon'
        },
        {
          name: '生产管理',
          iconSrc: 'production_icon'
        },
        {
          name: '库存管理',
          iconSrc: 'inventory_icon'
        },
        {
          name: '质量管理',
          iconSrc: 'quality_icon'
        },
        {
          name: '产品管理',
          iconSrc: 'product_icon'
        }
      ],
      enterpriseValue: [
        {
          name: '资源管控',
          iconSrc: 'resource_icon',
          imgSrc: 'product_1',
          abstract:
            '有效清洗企业供应链资料库，整合企业供应链上的人、财、物等资源，对企业供应链流程进行管理。'
        },
        {
          name: '生产优化',
          iconSrc: 'optimization_icon',
          imgSrc: 'product_2',
          abstract:
            '以精益生产、同步工程和敏捷制造的思维模式应对激烈的市场竞争，帮助企业与销售代理、客户与供应商形成利益共享的合作伙伴关系，运用“同步工程”组织生产，时刻保持产品的高质量，多样化和灵活性，实现精益生产。'
        },
        {
          name: '计划控制',
          iconSrc: 'plan_icon',
          imgSrc: 'product_3',
          abstract:
            '事先计划与事中控制，ERP系统中的计划体系主要包括生产计划、物料需求计划、能力需求计划等；ERP的控制体系充分考虑IT环境的影响，采取自顶向下的开发思路，兼顾整体与细节，循序渐进地不断完善。'
        },
        {
          name: '业务流程优化',
          iconSrc: 'process_icon',
          imgSrc: 'product_4',
          abstract:
            '改革、优化企业业务流程，建立一体化的组织管理，梳理和完善供应链运作流程，提高企业供应链的竞争优势。'
        }
      ]
    }
  }
  render() {
    const { erpIntro, productInfo, enterpriseValue } = this.state
    return (
      <div className="flex-column erpWrap">
        <div className="erpBannerWrap">
          <div className="iconSize">
            <h2>
              蜂巢<span>进销存</span>
            </h2>
            <h3>
              产品全生命周期流程监控，实现企业快速反应，全面提升核心竞争力
            </h3>
            <h4>
              <span>业务自动化 | 响应实时化 | 统计精准化</span>
            </h4>
          </div>
          {/* <img className="iconSize" src='https://obs-fe91.obs.cn-south-1.myhuaweicloud.com:443/%2Fhome%2Fbee%2Fproject%2Ftemp%2Fb09f9df906e14c568229915d79b24fb9.png' /> */}
        </div>
        <div className="flex-column erpIntroWrap">
          <div className="flex-column erpIntroCont">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">—</span>蜂巢进销存概述
                <span className="codeLine">—</span>
              </p>
              <p className="partnerName">Overview of Hive erp </p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            <div className="erpInfoText">{erpIntro}</div>
          </div>

          <div className="flex-column erpFunctionWrap">
            {productInfo && productInfo.length > 0 ? (
              <div className="flex-row erpFunction">
                {productInfo.map((productItem, productIndex) => {
                  return (
                    <div
                      className="flex-column erpFunctionItem"
                      key={`functionItem${productIndex}`}
                    >
                      <p className="functionIcon">
                        <img
                          className="iconSize"
                          src={imgGet(productItem.iconSrc, 'erp')}
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

        <div className="flex-column erpValue">
          <div className="flex-column erpValueContair">
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
              <div className="flex-row erpValueCont">
                {enterpriseValue.map((valueItem, valueIndex) => {
                  return (
                    <div
                      className="flex-column erpValueItem"
                      key={`value${valueIndex}`}
                    >
                      <img
                        className="picSize"
                        src={imgGet(valueItem.imgSrc, 'erp')}
                      />
                      <div className="flex-column erpValueInfo">
                        <div className="flex-column erpValueInfoCot">
                          <p className="erpProductIcon">
                            <img
                              className="erpProductSize"
                              src={imgGet(valueItem.iconSrc, 'erp')}
                            />
                          </p>
                          <p className="erpItemName">{valueItem.name}</p>
                          <p className="erpItemText">{valueItem.abstract}</p>
                        </div>
                      </div>
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
