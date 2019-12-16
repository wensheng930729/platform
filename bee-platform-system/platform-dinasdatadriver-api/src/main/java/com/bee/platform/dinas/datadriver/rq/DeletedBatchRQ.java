package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
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
@ApiModel("验货磅单批量删除请求参数")
public class DeletedBatchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("ids")
    @NotEmpty(message = "id集合不能为空")
    private List<Integer> ids;






}
