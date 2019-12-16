package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @Classname RepoReceiptDetail
 * @Description 入库明细
 * @Date 2019/5/28 16:50
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("成品出库明细请求参数")
public class RepoReceiptDetailProductOutRQ implements Serializable {

    private static final long serialVersionUID = 4181264684522476304L;

    @ApiModelProperty("成品出库明细id")
    private Integer id;

    @ApiModelProperty("关联的成品出库单据id")
    @NotNull(message = "关联的成品出库单据id不能为空")
    private Integer receiptId;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("产成品批次id")
    private Integer productBatchId;

    @ApiModelProperty("成品出库日期")
    @NotNull(message = "成品出库日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    @ApiModelProperty("车牌号")
    private String plateNo;

    @ApiModelProperty("毛重")
    @NotNull(message = "毛重不能为空")
    private BigDecimal roughWeight;

    @ApiModelProperty("皮重")
    private BigDecimal weight;


    @ApiModelProperty("计量单位")
    @NotEmpty(message = "计量单位不能为空")
    private String unit;

    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id不能为空")
    private Integer repositoryId;


    @ApiModelProperty("化验单id")
    @NotNull(message = "化验单id不能为空")
    private Integer testId;


//    @ApiModelProperty("出厂品位")
//    @NotNull(message = "出厂品位不能为空")
//    private BigDecimal grade;

    @ApiModelProperty("出库数量")
    @NotNull(message = "出库数量不能为空")
    @Min(value = 0,message = "出库数量不能小于0")
    private BigDecimal num;



}
