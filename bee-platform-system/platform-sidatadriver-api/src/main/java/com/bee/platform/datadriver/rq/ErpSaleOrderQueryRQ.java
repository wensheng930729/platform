package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.util.List;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("查询销售订单传参")
public class ErpSaleOrderQueryRQ implements Serializable{
    private static final long serialVersionUID = 1L;
	@ApiModelProperty("公司id")
    private Integer company;
    
	@ApiModelProperty("编号")
    private String contractNo;
    
	@ApiModelProperty("客户名称")
    private String customerName;
    
	@ApiModelProperty("产品名称")
    private String productName;
	
	@ApiModelProperty("开始时间")
    private String startTime;
	
	@ApiModelProperty("结束时间")
    private String endTime;

    @ApiModelProperty("当前用户所在企业及子企业id列表")
    private List<Integer> enterpriseIdList;
	
}
