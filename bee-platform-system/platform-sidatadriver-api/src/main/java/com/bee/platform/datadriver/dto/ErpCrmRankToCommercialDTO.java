package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpCrmRankToCommercialDTO
 * @Description 功能描述
 * @Date 2019/6/24 13:07
 **/
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("销售员排行榜到商机列表返回信息")
public class ErpCrmRankToCommercialDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("销售员id")
    private Integer saleUserId;

    @ApiModelProperty("销售员名称")
    private String saleUserName;

    @ApiModelProperty("客户id")
    private Integer customerId;

    @ApiModelProperty("客户名称")
    private String customerName;


    @ApiModelProperty("客户类型（码表取值）")
    private String customerType;


    @ApiModelProperty("当前阶段（码表取值）")
    private String phase;


    @ApiModelProperty("重要程度")
    private Integer degree;


    @ApiModelProperty("成交率")
    private Integer turnoverRatio;



}
