package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 领料出库主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("领料出库主表请求参数")
public class ErpOutOfStockOrderRQ implements Serializable{

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 领料出库编号
     */
    @ApiModelProperty("领料出库编号")
    @NotNull(message = "编号不能为空")
    private String code;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

    @ApiModelProperty("出库日期")
    @NotNull(message = "出库日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date outOfStockTime;
    /**
     * 料批id
     */
    @ApiModelProperty("料批id")
    @NotNull(message = "料批id不能为空")
    private Integer materialBatchId;
    /**
     * 炉号id
     */
    @ApiModelProperty("炉号id")
    @NotNull(message = "炉号id不能为空")
    private Integer furnaceNumberId;
    /**
     * 班次
     */
    @ApiModelProperty("班次")
    @NotEmpty(message = "班次不能为空")
    private String classes;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Length(max = 200,message = "备注不能超过200字")
    private String remark;







}
