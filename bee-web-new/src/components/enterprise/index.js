import React, { Component } from 'react'
import { Row, Col } from 'antd'
import { utils } from 'common'
import '../../common/styles/layout/index.less'
import '../../common/styles/layout/partner.less'
const { imgGet } = utils

//合作企业
export default class Part2 extends Component {
  constructor(props) {
    super(props)
    this.state = {}
  }

  render() {
    return (
      <div className="flex-column partnerWrap" justify="center">
        <div className="flex-column partnerMain">
          <div className="moduleTitle">
            <p>
              <span className="codeLine">—</span>合作伙伴
              <span className="codeLine">—</span>
            </p>
            <p className="partnerName">business partner</p>
          </div>
          <div className="partnerCont">
            {
              <img
                className="partnerImg"
                src={imgGet('enterprises', 'components/enterprise')}
              />
            }
          </div>
        </div>
      </div>
    )
  }
}
