package com.bee.platform.dinas.datadriver.rq;

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
import java.util.List;

/**
 * <p>
 * 采购结算表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("采购批量撤销结算请求参数")
public class CancelBatchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("ids")
    @NotEmpty(message = "id集合不能为空")
    private List<Integer> ids;






}
