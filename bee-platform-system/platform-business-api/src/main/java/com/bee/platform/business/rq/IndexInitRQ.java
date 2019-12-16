package com.bee.platform.business.rq;

import java.io.Serializable;
import java.math.BigDecimal;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "展示数据")
public class IndexInitRQ implements Serializable{

	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("首页-入驻企业")
    private Integer registerEnterprise;

    @ApiModelProperty("首页-完成订单数量")
    private Integer orderFulfill;

    @ApiModelProperty("首页-成交金额")
    private BigDecimal supplyMoney;

    @ApiModelProperty("集峰联运-合作商家")
    private Integer logisticalEnterpriseCnt;

    @ApiModelProperty("集峰联运-单量")
    private Integer logisticalServiceCnt;

    @ApiModelProperty("集峰联运-发布量（个）")
    private Integer publicRequirement;

    @ApiModelProperty("集峰联运-运量")
    private BigDecimal number9;

    @ApiModelProperty("线上峰贸-认证供应商(家)")
    private Integer countCompany;
    
    @ApiModelProperty("线上峰贸-最新采购询价(单)")
    private Integer countVaildInquiry;

    @ApiModelProperty("线上峰贸-报价中的供应商(家)")
    private Integer countVaildQuoted;

    @ApiModelProperty("线上峰贸-成交总额")
    private BigDecimal countMoney;


}
