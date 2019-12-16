package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 料批主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("根据id查询料批返回信息")
@JsonInclude
public class ErpMaterialBatchOrderDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 料批名称
     */
    @ApiModelProperty("料批名称")
    private String materialBatchName;
    /**
     * 产成品id
     */
    @ApiModelProperty("产成品id")
    private Integer productId;

    @ApiModelProperty("产成品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;
    /**
     * 产成品名称
     */
    @ApiModelProperty("产成品名称")
    private String productName;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
    private String companyName;
    /**
     * 数量
     */
    @ApiModelProperty("数量")
    private BigDecimal number;


    /**
     * 状态(0失效，1生效)
     */
    @ApiModelProperty("状态(0失效，1生效)")
    private Integer state;

    @ApiModelProperty("计量单位")
    private String unit;

    @ApiModelProperty("详情列表")
    List<ErpMaterialBatchOrderDetailDTO> detailList;


}
