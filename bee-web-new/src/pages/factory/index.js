import React, { Component } from 'react'
import { Button } from 'antd'
import { utils, go, openModal } from 'common'
import withRouter from 'umi/withRouter'
import '../../common/styles/layout/factory.less'
import '../../common/styles/theme/factory.less'

const { con } = go
const { imgGet } = utils
@withRouter
export default class Factory extends Component {
  constructor() {
    super()
    this.state = {
      intro:
        '通过与目标企业成立有限合伙公司，借助金蜜工业云平台的两大核心应用<br /> ——蜜云智造+领蜂供应链，集中力量帮助企业解决资金、用工、管理、原料等要素不足的系列难题，促进目标企业的可持续性经营',
      organization: 'organization',
      schemeOrganImg: 'scheme_organi1',
      enterpriseValue: [
        {
          title: '社会价值',
          name: '—',
          imgSrc: 'product_1',
          abstract:
            '借助金蜜工业云，实现社会责任与企业战略、业务的深度融合，缓解政府帮扶压力，促进工人就业、盘活产能、增加工业产值、实现企业与社会互利共赢。'
        },
        {
          title: '投资价值',
          name: '—',
          imgSrc: 'product_2',
          abstract: '债权保值，实现业务协同，稳定业务关系。'
        },
        {
          title: '企业价值',
          name: '—',
          imgSrc: 'product_3',
          abstract:
            '通过金蜜工业云助力企业数字化转型，提升管理效率、减少企业运转投入，实现复工复产。'
        }
      ]
    }
  }
  render() {
    const {
      intro,
      organization,
      schemeOrganImg,
      productInfo,
      enterpriseValue
    } = this.state
    return (
      <div className="flex-column factoryWrap">
        <div className="factoryBannerWrap">
          <div className="iconSize">
            <h2>
              <span>传统企业可持续经营赋能者</span>
            </h2>
            <h3>工厂运营</h3>
            <h4>
              <span>纵深企业现状 推进社会建设</span>
            </h4>
          </div>
          {/* <img className="iconSize" src={imgGet("banner", "factory")} /> */}
        </div>
        <div className="flex-column factoryIntroWrap">
          <div className="flex-column factoryIntroCont">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">—</span>工厂运营概述
                <span className="codeLine">—</span>
              </p>
              <p className="partnerName">Plant operation </p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            <div
              className="factoryInfoText"
              dangerouslySetInnerHTML={{ __html: intro }}
            />
            <div className="factoryOrganization">
              <img
                className="organizationSize"
                src={
                  'https://obs-fe91.obs.cn-south-1.myhuaweicloud.com:443/%2Fhome%2Fbee%2Fproject%2Ftemp%2F354f95b62f594a2fb2a336bc503f7698.png'
                }
              />
            </div>
          </div>
        </div>
        <div className="flex-column schemeOrganiWrap">
          <div className="flex-column schemeOrganiCont">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">—</span>工厂运营方案架构
                <span className="codeLine">—</span>
              </p>
              <p className="partnerName">Scheme Architecture</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            <div className="schemeOrganiPic">
              <img
                className="schemeSize"
                src={imgGet(schemeOrganImg, 'factory')}
              />
            </div>
          </div>
        </div>

        <div className="flex-column factoryValue">
          <div className="flex-column factoryValueContair">
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
              <div className="flex-row factoryValueCont">
                {enterpriseValue.map((valueItem, valueIndex) => {
                  return (
                    <div
                      className="flex-column factoryValueItem"
                      key={`value${valueIndex}`}
                    >
                      <img
                        className="picSize"
                        src={imgGet(valueItem.imgSrc, 'factory')}
                      />
                      <div className="flex-column factoryValueInfoCot">
                        <p className="factoryItemName">{valueItem.title}</p>
                        <p className="factoryItemCode">{valueItem.name}</p>
                        <p className="factoryItemText">{valueItem.abstract}</p>
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
