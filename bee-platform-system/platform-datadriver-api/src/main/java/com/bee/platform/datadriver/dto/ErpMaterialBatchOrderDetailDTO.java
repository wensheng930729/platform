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
 * 料批明细表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("详情列表")
@JsonInclude
public class ErpMaterialBatchOrderDetailDTO implements Serializable{

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 料批主表id
     */
    @ApiModelProperty("料批主表id")
    private Integer materialBatchOrderId;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    private Integer productId;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;
    /**
     * 数量
     */
    @ApiModelProperty("数量")
    private BigDecimal number;


}
