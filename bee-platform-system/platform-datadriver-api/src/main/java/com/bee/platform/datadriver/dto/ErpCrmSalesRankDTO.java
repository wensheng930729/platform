package com.bee.platform.datadriver.dto;

import cn.afterturn.easypoi.excel.annotation.Excel;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpCrmSalesRankDTO
 * @Description 功能描述
 * @Date 2019/6/24 10:52
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("销售员排行榜返回信息")
public class ErpCrmSalesRankDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */
    @ApiModelProperty("销售员id")
    private Integer saleUserId;

    @ApiModelProperty("营销人员名称")
    @Excel(name = "营销人员名称", orderNum = "1", width = 15)
    private String saleUserName;

    @ApiModelProperty("储备客户数量")
    @Excel(name = "储备客户数量", orderNum = "2", type = 10, width = 15)
    private Integer reserveCustomersNum;

    @ApiModelProperty("拜访客户数量")
    @Excel(name = "拜访客户数量", orderNum = "3", type = 10, width = 15)
    private Integer visitingCustomersNum;

    @ApiModelProperty("推进客户数量")
    @Excel(name = "推进客户数量", orderNum = "4", type = 10, width = 15)
    private Integer promotingCustomersNum;

    @ApiModelProperty("成交客户数量")
    @Excel(name = "成交客户数量", orderNum = "5", type = 10, width = 15)
    private Integer transactionClientsNum;

    @ApiModelProperty("是否可查看链接，true 可查看  false 不可查看")
    private Boolean viewMark=false;


}
