import React, { Component } from 'react'
import { Button } from 'antd'
import { utils,go,openModal } from 'common'
import withRouter from 'umi/withRouter'
import '../../../common/styles/layout/solutionsLocation.less'
import '../../../common/styles/theme/solutionsLocation.less'

const {con} = go
const { imgGet } = utils
@withRouter
export default class Location extends Component {
  constructor() {
    super()
    this.state = {
      bannerTitle:"高精度定位系统解决方案 ",
      bannerAbstract:"依托UWB室内定位系统，通过宽带脉冲通讯技术实现厘米级轨迹追踪，具备穿透力强、抗多<br />径效果好和安全性高等诸多优势，满足不同环境业务需求",
      intro:"系统采用UWB定位技术，基于到达时间差TDOA定位算法实现三维定位，单区域支持多于1000<br />张/秒的定位标签，精度高，容量大；支持终端访问，可为用户提供位置实时显示、历史轨迹<br />回放、人员考勤、电子围栏、行为分析、多卡判断、智能巡检等多项功能。 ",
      schemePlanImg:"scheme_plan",
      productInfo:[
        {
          title:"快速统计区域人数",
          abstract:"灵活掌控重点区域",
          iconSrc:"statistics"
        },
        {
          title:"检测异常行为",
          abstract:"实现预测报警",
          iconSrc:"test_icon"
        },
        {
          title:"扁平化信息推送",
          abstract:"兼具定向管控与个性化监管",
          iconSrc:"inform_push"
        }
      ],
      enterpriseValue:[
        {
          title:"养老行业",
          name:"—",
          imgSrc:"product_1",
          abstract:"医院/养老院人员定位系统实现了统一规范化的医护人员管理平台和病人监管平台，及时响应救援，保障病人、新生儿的人身安全，也增强了对医护人员行为的监管，形成良好的巡查习惯，减少医疗事故的发生。",
        },
        {
          title:"智能电厂",
          name:"—",
          imgSrc:"product_2",
          abstract:"电厂/变电站人员定位系统为电力能源行业实现了安全生产可控制、安全建设可控制和日常人员管理可控制和全面落实安全生产责任，确保实现安全生产的工作目标。",
        },
        {
          title:"工程隧道",
          name:"—",
          imgSrc:"product_3",
          abstract:"隧道/管廊/地铁人员定位系统支持全天候考勤，对施工人员进行实时自动跟踪，随时掌握每个员工在隧道的位置及活动轨迹、全隧道人员的位置分布。最重要的是，当遇到隧道突发事故，可以迅速找到被困人员可靠的位置信息，提高抢险救灾、安全救护的效率。",
        },
        {
          title:"监狱管理",
          name:"—",
          imgSrc:"product_4",
          abstract:"公安司法/监狱人员定位系统支持全天候点名，对犯人进行24小时位置监控和自动跟踪，随时掌握每个犯人的位置及活动轨迹、全监狱人员的位置分布，使监管工作智能化，提升立体防控能力，快速响应突发事件。",
        }
      ]
    }
  }
  render() {
    const { bannerTitle,bannerAbstract,intro,schemePlanImg,productInfo,enterpriseValue } = this.state;
    return (
      <div className="flex-column schematicPlanWrap">
        <div className="flex-column locationBannerWrap">
          <div className="flex-column LocationBannerInfo">
              <div className="LocationBannerTitle">{bannerTitle}</div>
              <div className="LocationBannerText" dangerouslySetInnerHTML={{__html:bannerAbstract}} />

              <div className="moduleTitle">
                 <p><span className="codeLine">——</span>详情和优势<span className="codeLine">——</span></p>
                 <p className="partnerName">Details and advantages</p>
                 <p className="productTitIcon">
                   <img className="productIconSize" src={imgGet("product_title_icon", "index")} />
                 </p>
              </div>
              <div className="brainIntroduce" dangerouslySetInnerHTML={{__html:intro}} />

          </div>

            {
              productInfo && productInfo.length > 0 ? (
              <div className="flex-row locationFunction">
                {
                  productInfo.map((productItem,productIndex)=>{
                    return (
                      <div className="flex-column locationFunctionItem" key={`functionItem${productIndex}`}>
                          <p className="functionIcon">
                              <img className="iconSize" src={imgGet(productItem.iconSrc, "solutions/location")} />
                          </p>
                          <p className="functionName">{productItem.title}</p>
                          <p className="functionInfo">{productItem.abstract}</p>
                      </div>
                    )
                  })
                }

              </div>
              ):null
            }
        </div>


        <div className="flex-column schematicPlanWrap">
          <div className="flex-column schematicPlanCont">
            <div className="moduleTitle">
               <p><span className="codeLine">—</span>高精度定位系统解决方案示意图<span className="codeLine">—</span></p>
               <p className="partnerName">Schematic plan</p>
               <p className="productTitIcon">
                 <img className="productIconSize" src={imgGet("product_title_icon", "index")} />
               </p>
            </div>
            <div className="schematicPlanPic">
                 <img className="schePlanSize" src={imgGet(schemePlanImg, "solutions/location")} />
            </div>
          </div>
        </div>

        <div className="flex-column locationValue">
          <div className="flex-column locationValueContair">
            <div className="moduleTitle">
               <p><span className="codeLine">—</span>应用场景<span className="codeLine">—</span></p>
               <p className="partnerName">Application scenarios</p>
               <p className="productTitIcon">
                 <img className="productIconSize" src={imgGet("product_title_icon", "index")} />
               </p>
            </div>
            {
              enterpriseValue && enterpriseValue.length > 0 ?(
                <div className="flex-row locationValueCont">
                  {
                    enterpriseValue.map((valueItem,valueIndex) =>{
                      return (
                        <div className="flex-column locationValueItem" key={`value${valueIndex}`}>
                           <img className="picSize" src={imgGet(valueItem.imgSrc, "solutions/location")} />
                             <div className="flex-column locationValueInfoCot">
                                <p className="locationItemName">{valueItem.title}</p>
                                <p className="locationItemCode">{valueItem.name}</p>
                                <p className="locationItemText">{valueItem.abstract}</p>
                              </div>

                        </div>
                      )
                    })
                  }

                </div>
              ):null
            }
           </div>
        </div>
    </div>
    )
  }
}
