package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpRepositoryReceiptProductOutSearchRQ
 * @Description 功能描述
 * @Date 2019/6/7 16:51
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("成品出库搜索列表请求参数")
public class ErpRepositoryReceiptProductOutSearchRQ implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("销售订单号")
    private String saleCode;

    @ApiModelProperty("客户")
    private String customer;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("出库单号")
    private String outCode;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

    private List<Integer>  list;

}
