import React, { Component } from 'react'
import { Button } from 'antd'
import { utils, go, openModal } from 'common'
import withRouter from 'umi/withRouter'
import '../../common/styles/layout/transport.less'
import '../../common/styles/theme/transport.less'

const { con } = go
const { imgGet } = utils
@withRouter
export default class Transport extends Component {
  constructor() {
    super()
    this.state = {
      intro:
        '集蜂联运致力于构建完善、高效的工业运输网络，通过提供公路、水路、铁路等多种优质服务项<br />目，更好地集成行业资源，统筹货运角色',
      serviceIntro:
        '集蜂联运拥有多个仓储中心，为企业量身定制仓储解决方案，利用最新技术对货物实现精准掌控，<br />高效解决企业多种现实痛点。',
      enterpriseValue: [
        {
          name: '运输可视',
          iconSrc: 'car_icon',
          abstract:
            '业务、货物信息无缝衔接，实现物流在途可视化，为物流运输提供优质高效的系统化服务。'
        },
        {
          name: '自动结算',
          iconSrc: 'settle_icon',
          abstract: '系统自动结算，账目明细自动生成，随时查看，清晰透明。'
        },
        {
          name: '科学管理',
          iconSrc: 'science_icon',
          abstract:
            '系统提供多种便捷操作，实现物流信息全生命周期的科学管理与监控。'
        },
        {
          name: '优质保障',
          iconSrc: 'ensure_icon',
          abstract:
            '运输全程紧密跟踪，异常情况及时处理，精准解决漏洞隐患，保障所有运输业务安全稳定。'
        }
      ]
    }
  }
  render() {
    const { intro, serviceIntro, enterpriseValue } = this.state
    return (
      <div className="flex-column transportWrap">
        <div className="transportBannerWrap">
          <div className="iconSize">
            <h2>集蜂联运</h2>
            <h3>资源多维集聚 | 货物在线寻源 | 交易精准透明</h3>
            <h4>
              <span>共享物流诚信共建平台经济</span>
            </h4>
          </div>
          {/* <img className="iconSize" src={imgGet("banner", "transport")} /> */}
        </div>

        <div className="flex-column transportIntroWrap">
          <div className="flex-column transportIntroCont">
            <div className="flex-column transportIntroInner">
              <div className="moduleTitle">
                <p>
                  <span className="codeLine">—</span>运输服务
                  <span className="codeLine">—</span>
                </p>
                <p className="partnerName">transportation services</p>
                <p className="productTitIcon">
                  <img
                    className="productIconSize"
                    src={imgGet('product_title_icon', 'index')}
                  />
                </p>
              </div>
              <div
                className="transportInfoText"
                dangerouslySetInnerHTML={{ __html: intro }}
              />
            </div>
          </div>
        </div>

        <div className="flex-column transportService">
          <div className="flex-column transportServiceCont">
            <div className="flex-column transportServiceInner">
              <div className="moduleTitle">
                <p>
                  <span className="codeLine">—</span>仓储服务
                  <span className="codeLine">—</span>
                </p>
                <p className="partnerName">Process introduction</p>
                <p className="productTitIcon">
                  <img
                    className="productIconSize"
                    src={imgGet('product_title_icon', 'index')}
                  />
                </p>
              </div>
              <div
                className="transportServiceText"
                dangerouslySetInnerHTML={{ __html: serviceIntro }}
              />
            </div>
          </div>
        </div>

        <div className="flex-column transportValue">
          <div className="flex-column transportValueContair">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">—</span>我们的优势
                <span className="codeLine">—</span>
              </p>
              <p className="partnerName">Service Advantage</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet('product_title_icon', 'index')}
                />
              </p>
            </div>
            {enterpriseValue && enterpriseValue.length > 0 ? (
              <div className="flex-row transportValueCont">
                {enterpriseValue.map((valueItem, valueIndex) => {
                  return (
                    <div
                      className="flex-column transportValueItem"
                      key={`value${valueIndex}`}
                    >
                      <p className="valueItemIcon">
                        <img
                          className="iconSize"
                          src={imgGet(valueItem.iconSrc, 'transport')}
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
