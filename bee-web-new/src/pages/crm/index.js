import React, { Component } from 'react'
import { Button } from 'antd'
import { utils, go, openModal } from 'common'
import withRouter from 'umi/withRouter'
import '../../common/styles/layout/crm.less'
import '../../common/styles/theme/crm.less'

const { con } = go
const { imgGet } = utils
@withRouter
export default class Crm extends Component {
  constructor() {
    super()
    this.state = {
      erpIntro:
        '一站式维护供应商，通过“角色设置 + 操作权限矩阵 + 数据权限范围”的三维矩阵 ，形成无限灵活的权限支撑，覆盖客户的全面需求，将客户关系管理提升至企业战略层面',
      productInfo: [
        {
          name: '客户档案管理',
          iconSrc: 'custorm_icon'
        },
        {
          name: '客户分类管理',
          iconSrc: 'itemize_icon'
        },
        {
          name: '商机管理',
          iconSrc: 'business_icon'
        },
        {
          name: '销售预测',
          iconSrc: 'sale_forecast_icon'
        },
        {
          name: '营销人员排行',
          iconSrc: 'sale_team_icon'
        }
      ],
      enterpriseValue: [
        {
          name: '显见的投资回报',
          iconSrc: 'investment_icon',
          abstract:
            '通过对客户详细资料的深入分析、客户关系的有效管理、客户价值的深度挖掘，加深企业对客户的了解，保持企业核心竞争力，留住老客户，吸引新客户，不断扩大企业投资回报。'
        },
        {
          name: '打破传统销售流程',
          iconSrc: 'sale_process_icon',
          abstract:
            '打破传统的企业销售流程，缩短产品销售周期，不断开发潜在客户，在开放透明的信息与资源的帮助下，销售人员通过分析交易中的重要信息，快速锁定客户需求，大幅提高交易成功率。'
        },
        {
          name: '客户知识共享',
          iconSrc: 'custorm_share_icon',
          abstract:
            '便捷、有效的客户信息库向企业各层人员提供了客户的相关信息，帮助企业进行正确的分析与判断，同时巩固企业与客户之间的联系，及时预判客户的潜在需求，提高客户满意度。'
        },
        {
          name: '销售预测',
          iconSrc: 'forecast_icon',
          abstract:
            '帮助企业管理者更加简捷地预测销售业绩，预估企业绩效。通过深入分析横向与纵向的销售趋势，探讨现有问题，掌握最新动态，挖掘潜在机会，增强企业的盈利能力。'
        }
      ]
    }
  }
  render() {
    const { erpIntro, productInfo, enterpriseValue } = this.state
    return (
      <div className="flex-column crmWrap">
        <div className="crmBannerWrap">
          <div className="iconSize">
            <h2>
              蜂客<span>CRM</span>
            </h2>
            <h3>全渠道智能化客户体验系统，提升客户满意度和市场份额</h3>
            <h4>
              <span>精细化商机管理 | 行业困境分析 | LTC线索到回款全流程</span>
            </h4>
          </div>
          {/* <img className="iconSize" src={imgGet("banner", "crm")} /> */}
        </div>
        <div className="flex-column crmIntroWrap">
          <div className="flex-column crmIntroCont">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">—</span>蜂客CRM概述
                <span className="codeLine">—</span>
              </p>
              <p className="partnerName">Overview of CRM</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            <div className="crmInfoText">{erpIntro}</div>
          </div>

          <div className="flex-column crmFunctionWrap">
            {productInfo && productInfo.length > 0 ? (
              <div className="flex-row crmFunction">
                {productInfo.map((productItem, productIndex) => {
                  return (
                    <div
                      className="flex-column crmFunctionItem"
                      key={`functionItem${productIndex}`}
                    >
                      <p className="functionIcon">
                        <img
                          className="iconSize"
                          src={imgGet(productItem.iconSrc, 'crm')}
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

        <div className="flex-column crmValue">
          <div className="flex-column crmValueContair">
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
              <div className="flex-row crmValueCont">
                {enterpriseValue.map((valueItem, valueIndex) => {
                  return (
                    <div
                      className="flex-column crmValueItem"
                      key={`value${valueIndex}`}
                    >
                      <div className="flex-column crmValueInfoCot">
                        <p className="crmProductIcon">
                          <img
                            className="crmProductSize"
                            src={imgGet(valueItem.iconSrc, 'crm')}
                          />
                        </p>
                        <p className="crmItemName">{valueItem.name}</p>
                        <p className="crmItemText">{valueItem.abstract}</p>
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
