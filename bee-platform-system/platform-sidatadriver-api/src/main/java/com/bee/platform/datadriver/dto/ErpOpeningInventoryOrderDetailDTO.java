package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 期初库存明细表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("期初库存明细返回信息")
@JsonInclude
public class ErpOpeningInventoryOrderDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 期初库存表id
     */
    @ApiModelProperty("期初库存表id")
    private Integer openingInventoryOrderId;
    /**
     * 产品_id
     */
    @ApiModelProperty("产品_id")
    private Integer productId;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;
    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    private Integer repositoryId;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    private String storeHouseName;
    /**
     * 计量单位
     */
    @ApiModelProperty("计量单位")
    private String unitOfMeasurement;
    /**
     * 化验单
     */
    @ApiModelProperty("化验单")
    private String testOrder;

    /**
     * 化验单id
     */
    @ApiModelProperty("化验单id")
    private Integer testReportId;
    /**
     * 期初数量
     */
    @ApiModelProperty("期初数量")
    private BigDecimal quantity;

    @ApiModelProperty("公司id")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("产成品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;

}
