import React, { Component } from "react";
import "common/styles/layout/smartSite.less";
import { utils, go, openModal } from "common";
const { imgGet } = utils;

export default class index extends Component {
  render() {
    return (
      <div className="smartSiteWrap">
        <div className="smartSiteContent">
          <div className="smartSiteBannerContent">
            <div className="smartSiteTitle">智慧工地系统解决方案</div>
            <div className="smartSiteLine"></div>
            <div className="smartSiteDesc">
              <p>
                利用信息化手段对工地进行智能化监管，通过安装各类传感器装置，构建智能监控和防范体系，进一步实现工厂的安全生产。
              </p>
            </div>
          </div>
          <div className="smartSiteBannerDetail">
            <div className="smartSiteBannerDetailTitle">
              <h3>
                <span className="smartSiteBannerDetailTitleLeft">——</span>
                详情和优势
                <span className="smartSiteBannerDetailTitleRight">——</span>
              </h3>
              <h4>Details and advantages</h4>
              <h5>
                <img
                  className="productIconSize"
                  src={imgGet("product_title_icon", "index")}
                />
              </h5>
            </div>
            <div className="smartSiteBannerDetailContent">
              <p>
                涵盖建筑企业自检系统、视频监控系统、起重机械安全管理系统、工地从业人员管理系统、环境监测系统、告警系统等六大子系统，通过构建分布式系统架构、监督评价分析模型，确保运行稳定可靠，协助政府职能部门、业主单位、施工单位对工程施工全过程实现有效管理。
              </p>
              <div className="smartSiteBannerDetailContentImg">
                <div className="smartSiteBannerDetailContentImgLeft smartSiteBannerDetailContentImgBox">
                  <p className="smartSiteBannerDetailLineOne">解决监管短板</p>
                  <p className="smartSiteBannerDetailLineTwo">
                    信息化建设不足等问题
                  </p>
                </div>
                <div className="smartSiteBannerDetailContentImgMiddle smartSiteBannerDetailContentImgBox">
                  <p className="smartSiteBannerDetailLineOne">回放隐蔽工程</p>
                  <p className="smartSiteBannerDetailLineTwo">实现全程监理</p>
                </div>
                <div className="smartSiteBannerDetailContentImgRight smartSiteBannerDetailContentImgBox">
                  <p className="smartSiteBannerDetailLineOne">
                  优化工程调度
                  </p>
                  <p className="smartSiteBannerDetailLineTwo">辅助科学决策</p>
                </div>
                <div className="smartSiteBannerDetailContentImgLast smartSiteBannerDetailContentImgBox">
                  <p className="smartSiteBannerDetailLineOne">
                  强化施工现场的动态监管
                  </p>
                  <p className="smartSiteBannerDetailLineTwo">提高施工质量</p>
                </div>
              </div>
            </div>
          </div>
          <div className="smartSiteSolutions">
            <h3>
              <span className="smartSiteSolutionsTitleLeft">——</span>
              智慧工地系统解决方案
              <span className="smartSiteSolutionsTitleRight">——</span>
            </h3>
            <h4>schematic</h4>
            <h5>
              <img
                className="productIconSize"
                src={imgGet("product_title_icon", "index")}
              />
            </h5>
            <div className="smartSiteSolutionsImg" />
          </div>
        </div>

        {/* <div className='smartSiteExample'>
                    <div className='smartSiteExampleContent'>
                    <h3><span className='smartSiteExampleContentTitleLeft'>——</span>能源管控系统应用案例<span className='smartSiteExampleContentTitleRight'>——</span></h3>
                            <h4>Application cases</h4>
                            <h5><img className="productIconSize" src={imgGet("product_title_icon", "index")} /></h5>
                            <div className='smartSiteExampleContentDetail'>
                                <div className='smartSiteExampleContentDetailLeft'>
                                    <h3>项目概述</h3>
                                    <p>
                                    四川某硅业有限公司作为国内较早从事硅冶炼的民营企业之一，年产值达数亿元，电力耗能巨大。为进一步提高电能使用率和企业生产效率，该公司与金蜜工业云进行深度合作，采用蜂创物联的智能电表管理系统，通过智能电表进行数据采集，在数据上报金蜜工业云后，将数据进行统计并分析，灵活反映线路损耗情况、设备用电情况，对电能数据进行智能计量和自动监测，助力企业实现节能降耗。
                                    </p>
                                    <div className='smartSiteAsk'>方案咨询</div>
                                </div>
                                <div className='smartSiteExampleContentDetailRight'>

                                </div>
                            </div>
                    </div>
                </div> */}
      </div>
    );
  }
}
