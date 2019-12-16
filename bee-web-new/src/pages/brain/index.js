import React, { Component } from 'react'
import { Button } from 'antd'
import { utils, go, openModal } from 'common'
import withRouter from 'umi/withRouter'
import '../../common/styles/layout/brain.less'
import '../../common/styles/theme/brain.less'

const { con } = go
const { imgGet } = utils
@withRouter
export default class Brain extends Component {
  constructor() {
    super()
    this.state = {
      brainIntro:
        '通过蜜云智造全景连接人、机、料、法、环，实现一人一物一码、生产实时监控、工作智能推送；建立工业行业的知识图谱，内管生产，外测行业，用数据构建全新工业大脑',
      productInfo: [
        {
          name: '智能地磅',
          iconSrc: 'purchase_icon1'
        },
        {
          name: '随身质检',
          iconSrc: 'finance_icon1'
        },
        {
          name: '条码跟踪',
          iconSrc: 'manage_icon1'
        },
        {
          name: '智能排班',
          iconSrc: 'person_icon1'
        },
        {
          name: '流程可视',
          iconSrc: 'sale_icon1'
        },
        {
          name: '蜜优BOM',
          iconSrc: 'BOM_icon'
        }
      ],
      enterpriseValue: [
        {
          name: '行情一网打尽',
          iconSrc: 'computer',
          abstract:
            '政策跟踪观察，发展趋势准确把握，竞争格局深入洞悉，行业行情一网打尽。'
        },
        {
          name: '全生命周期可追溯',
          iconSrc: 'plane1',
          abstract:
            '建立生产、物流、销售的可信流通体系，实现产品生产过程全生命周期可追溯。'
        },
        {
          name: '工作流程自动化跟踪',
          iconSrc: 'folder1',
          abstract:
            '跟进、催办、查看，归类存档和检索，合同执行情况、自动化流程可跟踪。'
        },
        {
          name: '降本增效',
          iconSrc: 'shield',
          abstract:
            '打造销售、生产、策划互动的网络系统，实现全部门联动，实现利润最大化。'
        }
        // {
        //   name: '辅助决策',
        //   iconSrc: 'head',
        //   abstract:
        //     '支持数据挖掘与智能钻取，对采购、销售、财务等关键业务指标进行多维数据分析，全方位支撑领航者科学决策。'
        // }
      ]
    }
  }
  render() {
    const { brainIntro, productInfo, enterpriseValue } = this.state
    return (
      <div className="flex-column brainWrap">
        <div className="bannerWrap">
          <div className="flex-column bannerBaseInfo">
            <div className="flex-column bannerBaseInfoText">
              <p className="arrowDown">
                <img
                  className="arrowSize"
                  src={imgGet('arrow_down', 'brain')}
                />
              </p>
              <div className="moduleTitle">
                <p>
                  <span className="codeLine">——</span>蜜云智造概述
                  <span className="codeLine">——</span>
                </p>
                <p className="partnerName">Overview</p>
                <p className="productTitIcon">
                  <img
                    className="productIconSize"
                    src={imgGet('product_title_icon', 'index')}
                  />
                </p>
              </div>
              <p className="brainIntroduce">{brainIntro}</p>
            </div>
          </div>
        </div>

        <div className="flex-column brainProductContair">
          {productInfo && productInfo.length > 0 ? (
            <div className="flex-row brainFunction">
              {productInfo.map((productItem, productIndex) => {
                return (
                  <div
                    className="flex-column braninFunctionItem"
                    key={`functionItem${productIndex}`}
                  >
                    <p className="functionIcon">
                      <img
                        className="iconSize"
                        src={imgGet(productItem.iconSrc, 'brain')}
                      />
                    </p>
                    <p className="functionName">{productItem.name}</p>
                  </div>
                )
              })}
            </div>
          ) : null}

          <div className="flex-column brainProcess">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">——</span>蜜云智造架构
                <span className="codeLine">——</span>
              </p>
              <p className="partnerName">Architecture</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            <div className="flex-column brainProcess">
              <img className="processSize" src={imgGet('process2', 'brain')} />
            </div>
          </div>
        </div>
        <div className="flex-column brainValue">
          <div className="flex-column brainValueContair">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">——</span>我们能为企业带来的价值
                <span className="codeLine">——</span>
              </p>
              <p className="partnerName">value</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            {enterpriseValue && enterpriseValue.length > 0 ? (
              <div className="flex-row brainValueCont">
                {enterpriseValue.map((valueItem, valueIndex) => {
                  return (
                    <div
                      className="flex-column brainValueItem"
                      key={`value${valueIndex}`}
                    >
                      <p className="valueItemIcon">
                        <img
                          className="iconSize"
                          src={imgGet(valueItem.iconSrc, 'brain')}
                        />
                      </p>
                      <p className="valueItemName">{valueItem.name}</p>
                      <p className="valueItemText">{valueItem.abstract}</p>
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
