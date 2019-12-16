package com.bee.platform.datadriver.dto;

import java.io.Serializable;

import cn.afterturn.easypoi.excel.annotation.Excel;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("商机客户导出返回的信息")
public class ErpCrmCommercialBOCDTO implements Serializable{
	
	/**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */
    private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("销售员id")
    private Integer saleUserId;

    @ApiModelProperty("名称销售员")
    @Excel(name = "营销人员名称", orderNum = "1", width = 15)
    private String saleUserName;
    
    @ApiModelProperty("客户名称")
    @Excel(name = "客户名称", orderNum = "2", width = 15)
    private String customerName;
   
    @ApiModelProperty("客户类型")
    @Excel(name = "客户类型", orderNum = "3", width = 15)
    private String customerType;
    
    @ApiModelProperty("所属行业")
    @Excel(name = "所属行业", orderNum = "4", width = 15)
    private String industry;
    
    @ApiModelProperty("获客渠道（码表取值）")
    @Excel(name = "获客渠道", orderNum = "5", width = 15)
    private String customerObtainMethod;
    
    @ApiModelProperty("详细地址")
    @Excel(name = "详细地址", orderNum = "6", width = 15)
    private String address;
    
    @ApiModelProperty("联系人")
    @Excel(name = "联系人", orderNum = "7", width = 15)
    private String contactName;
    
    @ApiModelProperty("联系方式")
    @Excel(name = "联系方式", orderNum = "8", width = 15)
    private String contactPhone;
    
    @ApiModelProperty("当前阶段")
    @Excel(name = "当前阶段", orderNum = "9", width = 15)
    private String phase;

    @ApiModelProperty("重要程度")
    @Excel(name = "重要程度(单位：星)", orderNum = "10", type = 10, width = 15)
    private Integer degree;
   
    @ApiModelProperty("成交率")
    @Excel(name = "成交率(单位：星)", orderNum = "11", type = 10, width = 15)
    private Integer turnoverRatio;
    
    
}
