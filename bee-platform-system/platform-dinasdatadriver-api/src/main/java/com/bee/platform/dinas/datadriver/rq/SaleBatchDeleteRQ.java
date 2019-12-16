package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/8/19
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("批量删除请求信息")
public class SaleBatchDeleteRQ implements Serializable {
    private static final long serialVersionUID = -6378576271321000647L;

    @NotNull(message = "待删除的id列表不能为空")
    @ApiModelProperty("待删除的id列表")
    private List<Integer> ids;
}
