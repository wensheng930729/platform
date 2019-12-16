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
 * @ClassName ErpRepositoryReceiptRawInSearchRQ
 * @Description 功能描述
 * @Date 2019/6/7 16:01
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("原料入库搜索列表请求参数")
public class ErpRepositoryReceiptRawInSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("采购订单号")
    private String purchaseCode;

    @ApiModelProperty("供应商")
    private String supplier;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("入库单号")
    private String inCode;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

    private List<Integer> list;

}
