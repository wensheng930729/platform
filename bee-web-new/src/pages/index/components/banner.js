import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { utils } from 'common'
import styles from '../../../common/styles/theme/home.less'
const { imgGet } = utils

export default class Banner extends Component {
  constructor(props) {
    super(props)
    this.state = {
      companyCulture: [
        {
          key: 1,
          name: '过程可控',
          abstract:
            '人，机，料，法，环。达到一人一物一码，实现生产过程实时可见可控',
          img: 'businessIcon1',
          cultureWt: 'cultureWt1'
        },
        {
          key: 2,
          name: '全景计算',
          abstract:
            '利用人工智能，云计算等先进技术，结合内外部大数据，构建工业行业知识图谱',
          img: 'visualizationIcon1',
          cultureWt: 'cultureWt2'
        },
        {
          key: 3,
          name: '数据决策',
          abstract:
            '内管生产，外测行业，打造产供销一体化管理流程，辅助科学决策',
          img: 'intelligentIcon',
          cultureWt: 'cultureWt3'
        }
      ]
    }
  }
  render() {
    const { companyCulture } = this.state
    return (
      <div className="flex-column bannerWrap">
        <i className="earthRotation" />
        <i className="bannerMask" />
        <i className="meteorArrow" />
        <i className="meteorArrow meteorArrow2" />
        <i className="meteorArrow meteorArrow3" />
        <div className="flex-column companyCulture">
          <div className="flex-row companyCultureList">
            {companyCulture && companyCulture.length > 0
              ? companyCulture.map((item, index) => {
                  return (
                    <div
                      className={[
                        `flex-row companyCultureItem ${item.cultureWt}`
                      ]}
                      key={`productValue${index}`}
                    >
                      <div className="cultureItemIcon">
                        <img
                          className="cultureIcon"
                          src={imgGet(item.img, 'index')}
                        />
                      </div>
                      <div className="cultureItemInfo">
                        <h3 className="cultureItemTitle">{item.name}</h3>
                        <p className="cultureItemText"> {item.abstract}</p>
                      </div>
                    </div>
                  )
                })
              : null}
          </div>
        </div>
      </div>
    )
  }
}
